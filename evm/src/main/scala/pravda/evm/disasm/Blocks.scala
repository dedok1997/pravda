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

package pravda.evm.disasm

import pravda.evm.EVM
import pravda.evm.EVM._
import pravda.evm.translate.Translator.{ActualCode, Addressed, ContractCode, CreationCode}

import scala.annotation.tailrec

object Blocks {

  def split(ops: List[Op]): List[List[Op]] = {
    @tailrec def split(ops: List[Op], acc: List[List[Op]]): List[List[Op]] = ops match {
      case Nil                        => acc.reverse
      case JumpDest :: xs             => split(xs, List(JumpDest) :: acc)
      case JumpDest(addr) :: xs       => split(xs, List(JumpDest(addr)) :: acc.head.reverse :: acc.tail)
      case SelfAddressedJump(n) :: xs => split(xs, Nil :: (SelfAddressedJump(n) :: acc.head).reverse :: acc.tail)
      case SelfAddressedJumpI(n) :: xs =>
        split(xs, (SelfAddressedJumpI(n) :: Nil) :: (SelfAddressedJumpI(n) :: acc.head).reverse :: acc.tail)

      case Jump(addr, dest) :: xs => split(xs, Nil :: (Jump(addr, dest) :: acc.head).reverse :: acc.tail)
      case JumpI(addr, dest) :: xs =>
        split(xs, (JumpI(addr, dest) :: Nil) :: (JumpI(addr, dest) :: acc.head).reverse :: acc.tail)
      case Return :: xs       => split(xs, Nil :: (Return :: acc.head).reverse :: acc.tail)
      case SelfDestruct :: xs => split(xs, Nil :: (SelfDestruct :: acc.head).reverse :: acc.tail)
      case Stop :: xs         => split(xs, Nil :: (Stop :: acc.head).reverse :: acc.tail)
      case Invalid :: xs      => split(xs, Nil :: (Invalid :: acc.head).reverse :: acc.tail)
      case Revert :: xs       => split(xs, Nil :: (Revert :: acc.head).reverse :: acc.tail)

      case x :: xs => split(xs, (x :: acc.head) :: acc.tail)
    }
    split(ops, List(Nil)).filter(_.nonEmpty)
  }

  case class WithJumpDest(dest: JumpDest, ops: List[Op])
  case class WithJumpI(jumpi: SelfAddressedJumpI, ops: List[Op])
  case class Jumpable(withJumpdest: List[WithJumpDest], withoutJumpdest: List[List[Op]])

  def jumpable(blocks: List[List[Op]]): Jumpable = {
    val (withJ, withoutJ) = blocks.partition {
      case JumpDest(addr) :: xs => true
      case xs                   => false
    }
    Jumpable(withJ.collect {
      case JumpDest(addr) :: xs => WithJumpDest(JumpDest(addr), xs)
    }, withoutJ)
  }

  def continuation(blocks: List[List[Op]]): List[WithJumpI] = {
    blocks.collect {
      case SelfAddressedJumpI(addr) :: xs => WithJumpI(SelfAddressedJumpI(addr), xs)
    }
  }

  def splitToCreativeAndRuntime(ops: List[Addressed[EVM.Op]]): Either[String, ContractCode] = {
    val length = ops.last._1.toLong
    val blocks = Blocks.split(ops.map(_._2))
    val offsetOpt: Option[Int] = blocks
      .map(bl => Emulator.eval(bl, new StackList(List.empty[StackItem]), List.empty[HistoryRecord]))
      .flatMap { case (s, h) => h.collect { case r @ HistoryRecord(CodeCopy, _ :: Number(n) :: _) => n } }
      .find(_ < length)
      .map(_.toInt)

    offsetOpt.map { offset =>
      val (creative, runtime) = ops.partition(_._1 - offset < 0)

      val addressedCreative = creative.map {
        case (ind, JumpDest) => ind -> JumpDest(ind)
        case (ind, x)        => ind -> x
      }

      val addressedRuntime = runtime.map {
        case (ind, JumpDest(_))           => (ind - offset) -> JumpDest(ind - offset)
        case (ind, JumpDest)              => (ind - offset) -> JumpDest(ind - offset)
        case (ind, SelfAddressedJump(_))  => (ind - offset) -> SelfAddressedJump(ind - offset)
        case (ind, SelfAddressedJumpI(_)) => (ind - offset) -> SelfAddressedJumpI(ind - offset)
        case (ind, Jump)                  => (ind - offset) -> SelfAddressedJump(ind - offset)
        case (ind, JumpI)                 => (ind - offset) -> SelfAddressedJumpI(ind - offset)

        case (ind, x) => (ind - offset) -> x
      }
      (CreationCode(addressedCreative), ActualCode(addressedRuntime))
    } toRight "Can't split to creative and runtime"
  }

}
