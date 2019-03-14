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
import pravda.evm.translate.Translator._
import pravda.evm.translate.opcode._
import pravda.vm.asm.Operation
import pravda.vm.{Meta, asm}
import cats.implicits._
import pravda.evm.translate.Translator

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
                                   abi: List[AbiObject]): Either[String, List[asm.Operation]] =
    Translator.translateActualContract(ops,abi,apply)

}
