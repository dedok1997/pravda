/*
 * Copyright (C) 2018  Expload.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package pravda.evm.translate

import cats.instances.list._
import cats.instances.either._
import cats.syntax.traverse._
import pravda.evm.EVM
import pravda.evm.abi.parse.AbiParser
import pravda.evm.abi.parse.AbiParser.AbiObject
import pravda.evm.disasm.{Blocks, JumpTargetRecognizer, StackSizePredictor}
import pravda.evm.function.CodeGenerator
import pravda.evm.translate.opcode._
import pravda.vm.asm.Operation
import pravda.vm.{Data, Opcodes, asm}

import scala.annotation.tailrec

object Translator {

  trait EvmCode {
    def code: List[Addressed[EVM.Op]]
  }

  case class CreationCode(code: List[Addressed[EVM.Op]]) extends EvmCode
  case class ActualCode(code: List[Addressed[EVM.Op]])   extends EvmCode
  case class Code(code: List[Addressed[EVM.Op]])         extends EvmCode

  type Converted = Either[EVM.Op, List[asm.Operation]]
  type Addressed[T] = (Int, T)
  type ContractCode = (CreationCode, ActualCode)

  val startLabelNameWithPravdaSpec = "__start_evm_program_pravda_spec"
  val startLabelNameWithEvmSpec = "__start_evm_program_evm_spec"
  val startEvmFuncSelect = "start_evm_func_select"

  val defaultMemorySize = 1024

  def apply(ops: List[EVM.Op], abi: List[AbiObject]): Either[String, List[asm.Operation]] = {
    val (funcs, _, _) = AbiObject.unwrap(abi)
    FunctionSelectorTranslator
      .evmToOps(ops, funcs)
      .map {
        case Left(op)     => SimpleTranslation.evmOpToOps(op)
        case Right(value) => Right(value)
      }
      .map(_.left.map(op => s"incorrect op: ${op.toString}"))
      .sequence
      .map(_.flatten)
  }

  @tailrec def filterCode(ops: List[EVM.Op],acc: List[EVM.Op] = Nil): List[EVM.Op] = {
    import fastparse.byte.all.Bytes
    import pravda.evm.EVM._

    ops match {
      case Push(Bytes(-128)) ::
            Push(Bytes(0x40)) ::
            MStore(2) ::
            rest =>
        filterCode(Push(Bytes(-128)) :: Push(Bytes(0x40)) :: MStore(3) :: rest,acc)
      case Push(Bytes(0x04)) ::
            CallDataSize(1) ::
            Lt ::
            Push(_: Bytes) ::
            JumpI(_,_)
            :: rest =>
        filterCode(rest,acc)
      case Push(Bytes(0x00)) ::
            CallDataLoad(1) ::
            Push(bs1: Bytes) ::
            Swap(1) ::
            Div ::
            Push(bs2: Bytes) ::
            And ::
            rest
          if bs1 == Bytes.fromHex("0x0100000000000000000000000000000000000000000000000000000000").get &&
            bs2 == Bytes.fromHex("0xffffffff").get =>
        filterCode(rest,acc)
      case Push(Bytes(0x00)) ::
            CallDataLoad(1) ::
            Push(bs1: Bytes) ::
            Swap(1) ::
            Div ::
            rest if bs1 == Bytes.fromHex("0x0100000000000000000000000000000000000000000000000000000000").get =>
        filterCode(rest,acc)
      case CallValue ::
            Dup(1) ::
            IsZero ::
            Push(_) ::
            JumpI(addr,dest) ::
            Push(Bytes(0x00)) ::
            Dup(1) ::
            Revert :: rest =>
        filterCode(Push(Bytes(0x7)) :: Push(Bytes(0x7)) :: Jump(addr, dest) :: rest,acc)

      case h :: t => filterCode(t,h :: acc)
      case _      => acc.reverse
    }
  }

  def translateActualContract(ops: List[Addressed[EVM.Op]],
                              abi: List[AbiObject],
                              translator: (List[EVM.Op], List[AbiObject]) => Either[String, List[asm.Operation]]): Either[String, List[asm.Operation]] = {
    import pravda.evm.EVM._
    import pravda.evm.function.CodeGenerator._

    for {
      code1 <- Blocks.splitToCreativeAndRuntime(ops)
      (creationCode1, actualContract1) = code1
      (jumps,code2) =  JumpTargetRecognizer(actualContract1)
      ops = StackSizePredictor.clear(StackSizePredictor.emulate(code2.map(_._2),jumps))
      filtered = filterCode(ops)

      jumpDests = filtered.collect{case j@JumpDest(addr) => j}.zipWithIndex
      prepared = JumpDestinationPrepare.prepared(jumpDests)
      hashes = FunctionSelectorTranslator.hashes(filtered)
      res <- translator(filtered, abi).map(
        opcodes =>
          createArray(defaultMemorySize) ~
          swap(1) ~
          dup(1) ~
          push(Data.Primitive.Null) ~
          CodeGenerator.eq ~
          not ~
          jumpi(Some(startLabelNameWithPravdaSpec)) ~
          pop ~

          swap(1) ~ dup(1) ~ length ~ pushInt(32) ~ gt ~ not ~
            jumpi(Some(startEvmFuncSelect)) ~
            expand ~
            label(startEvmFuncSelect) ~
            swap(1) ~
            FunctionSelectorTranslator.byFirstBytesSelector(hashes) ~
          prepared ~
          label(startLabelNameWithPravdaSpec) ~
          opcodes ~
          StdlibAsm.stdlibFuncs.flatMap(_.code) ~
          convertResult(abi)
      )
    } yield res
  }

  def convertResult(abi: List[AbiObject]): List[Operation] = {
    val (funcs, _, _) = AbiObject.unwrap(abi)

    def castResult(arg: AbiParser.Argument): List[Operation] = AbiParser.nameToType(arg.`type`) match {
      case EVM.UInt(num) =>
        num match {
          case 8                 => cast(Data.Type.Int16)
          case 16 | 24           => cast(Data.Type.Int32)
          case 32 | 40 | 48 | 56 => cast(Data.Type.Int64)
          case _                 => cast(Data.Type.BigInt)
        }
      case EVM.SInt(num) =>
        num match {
          case 8                 => cast(Data.Type.Int8)
          case 16                => cast(Data.Type.Int16)
          case 24 | 32           => cast(Data.Type.Int32)
          case 40 | 48 | 56 | 64 => cast(Data.Type.Int64)
          case _                 => cast(Data.Type.BigInt)
        }
      case EVM.Bool        => cast(Data.Type.Boolean)
      case EVM.Unsupported => List.empty
    }

    List(Operation.Label("convert_result")) ++ funcs.flatMap { f =>
      List(
        Operation(Opcodes.DUP),
        Operation.Push(Data.Primitive.Utf8(f.name)),
        Operation(Opcodes.EQ),
        Operation(Opcodes.NOT),
          Operation.JumpI(Some(s"convert_result_not_${f.name}"))
      ) ++ List(Operation(Opcodes.POP)) ++
        f.outputs.headOption.map(h => castResult(h)).toList.flatten ++
        List(Operation(Opcodes.STOP), Operation.Label(s"convert_result_not_${f.name}"))
    }
  }
}
