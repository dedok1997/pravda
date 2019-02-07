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

package pravda.evm.debug.evm

import pravda.evm.EVM
import pravda.evm.abi.parse.AbiParser.AbiObject
import pravda.evm.disasm.{Blocks, StackSizePredictor}
import pravda.evm.translate.Translator._
import pravda.evm.translate.opcode._
import pravda.vm.asm.Operation
import pravda.vm.{Meta, Opcodes, asm}
import cats.implicits._
import pravda.evm.EVM.JumpDest

object EvmDebugTranslator {

  val debugMarker = "evm_debug_"

  def apply(ops: List[EVM.Op], abi: List[AbiObject]): Either[String, List[asm.Operation]] = {
    val (funcs, _, _) = AbiObject.unwrap(abi)
    FunctionSelectorTranslator
      .evmToOps(ops, funcs)
      .map {
        case Left(op) =>
          SimpleTranslation.evmOpToOps(op).map(l => Operation.Meta(Meta.Custom(debugMarker + op.toString)) :: l)
        case Right(value) => Right(Operation.Meta(Meta.Custom(debugMarker + "Function selection opcode")) :: value)
      }
      .map(_.left.map(op => s"incorrect op: ${op.toString}"))
      .sequence
      .map(_.flatten)
  }

  def debugTranslateActualContract(ops: List[Addressed[EVM.Op]],
                                   abi: List[AbiObject]): Either[String, List[asm.Operation]] = {
    for {
      code1 <- Blocks.splitToCreativeAndRuntime(ops)
      (creationCode1, actualContract1) = code1
//      code = JumpTargetRecognizer(actualContract1)
//      //code = actualContract1.code
//      _ = code.left.foreach{
//        case (s1,s2) =>
//          s1.foreach(println)
//          println("_" * 10)
//          s2.foreach(println)
//      }
//      _ = actualContract1.code.foreach(println)
  //    code2 <- code.left.map(_.toString)
      code2 = actualContract1.code
      ops1 = StackSizePredictor.emulate(code2.map(_._2))
      ops = StackSizePredictor.clear(ops1)
      filtered = filterCode(ops)
      jumpDests = filtered.collect{case j@JumpDest(addr) => j}.zipWithIndex
      prepared = JumpDestinationPrepare.prepared(jumpDests)

      res <- EvmDebugTranslator(filtered, abi).map(
        opcodes =>
        prepared :::
          Operation.Label(startLabelName) ::
            createArray(defaultMemorySize) :::
            Operation(Opcodes.SWAP) ::
            opcodes :::
            StdlibAsm.stdlibFuncs.flatMap(_.code) :::
            convertResult(abi)
      )
    } yield res
  }

}
