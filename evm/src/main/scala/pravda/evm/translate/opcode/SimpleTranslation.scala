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

package pravda.evm.translate.opcode

import pravda.evm.EVM
import pravda.vm.asm
import pravda.vm._

object SimpleTranslation {

  import pravda.evm.EVM._

  private def codeToOps(codes: Int*) = codes.map(asm.Operation(_)).toList

  private val translate: PartialFunction[EVM.Op, List[asm.Operation]] = {
    case Push(bytes) => pushBigInt(BigInt(1, bytes.toArray)) :: Nil

    case Pop => codeToOps(Opcodes.POP)

    case Add => codeToOps(Opcodes.ADD) //FIXME result % 2^256
    case Mul => codeToOps(Opcodes.MUL)
    case Div => codeToOps(Opcodes.DIV) //FIXME 0 if stack[1] == 0 othervise s[0] / s[1]
    case Mod => codeToOps(Opcodes.MOD) //FIXME 0 if stack[1] == 0 othervise s[0] % s[1]
    case Sub => sub //FIXME result & (2^256 - 1)
    case AddMod =>
      dupn(3) ::: codeToOps(Opcodes.SWAP, Opcodes.MOD, Opcodes.SWAP) ::: dupn(3) ::: codeToOps(Opcodes.SWAP,
                                                                                               Opcodes.MOD,
                                                                                               Opcodes.ADD,
                                                                                               Opcodes.MOD)
    case MulMod =>
      dupn(3) ::: codeToOps(Opcodes.SWAP, Opcodes.MOD, Opcodes.SWAP) ::: dupn(3) ::: codeToOps(Opcodes.SWAP,
                                                                                               Opcodes.MOD,
                                                                                               Opcodes.MUL,
                                                                                               Opcodes.MOD)

    //  case Not => codeToOps(Opcodes.NOT) //TODO (2^256 - 1) - s[0]
    case And => codeToOps(Opcodes.AND)
    case Or  => codeToOps(Opcodes.OR)
    case Xor => codeToOps(Opcodes.XOR)

    case Byte =>
      List(
        pushBigInt(31) :: Nil,
        sub,
        pushBigInt(8) :: Nil,
        codeToOps(Opcodes.MUL),
        pushBigInt(2) :: Nil,
        callExp,
        codeToOps(Opcodes.SWAP),
        codeToOps(Opcodes.DIV),
        pushInt(0xff) :: Nil,
        codeToOps(Opcodes.AND)
      ).flatten

    case IsZero => pushBigInt(BigInt(0)) :: codeToOps(Opcodes.EQ) ::: cast(Data.Type.BigInt)
    case Lt     => codeToOps(Opcodes.LT) ::: cast(Data.Type.BigInt)
    case Gt     => codeToOps(Opcodes.GT) ::: cast(Data.Type.BigInt)
    case Eq     => codeToOps(Opcodes.EQ) ::: cast(Data.Type.BigInt)

    //case Jump => codeToOps(Opcodes.JUMP)
    //case JumpI => codeToOps(Opcodes.SWAP) ::: cast(Data.Type.Boolean) ::: codeToOps(Opcodes.JUMP)
    case Stop => codeToOps(Opcodes.STOP)

    case Dup(n)  => if (n > 1) dupn(n) else codeToOps(Opcodes.DUP)
    case Swap(n) => if (n > 1) swapn(n + 1) else codeToOps(Opcodes.SWAP)

    case Balance => codeToOps(Opcodes.BALANCE)
    case Address => codeToOps(Opcodes.PADDR)
  }

  def evmOpToOps(op: EVM.Op): Either[String, List[asm.Operation]] =
    translate.lift(op).toRight(s"Unknown opcode $op")
}
