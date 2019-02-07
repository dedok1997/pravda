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

import scala.annotation.tailrec

trait StackWithNegativeSize[A] { self =>

  def pop(n: Int): (List[A], StackWithNegativeSize[A])
  def pop(): (Option[A], StackWithNegativeSize[A])
  def push[B >: A](a: B): StackWithNegativeSize[B]
  def swap(a: Int): StackWithNegativeSize[A]
  def dup(a: Int): StackWithNegativeSize[A]
  def size: Int
}

case class StackList[A](state: List[A],size: Int) extends StackWithNegativeSize[A] {self =>

  override def pop(n: Int): (List[A], StackWithNegativeSize[A]) =  {
    @tailrec def aux(n: Int, acc: (List[A], StackWithNegativeSize[A])): (List[A], StackWithNegativeSize[A]) = n match {
      case 0 => acc._1.reverse -> acc._2
      case _ =>
        val (elO, tail) = acc._2.pop()
        elO match {
          case Some(el) => aux(n - 1, (el :: acc._1, tail))
          case _ => throw new Exception
        }
    }
    if (n <= size) aux(n, (Nil, self)) else  state -> new StackList[A](List.empty,size - n)
  }

  def pop(): (Option[A], StackWithNegativeSize[A]) =
    if(size > 0)
      Some(state.head) -> new StackList(state.tail,size - 1)
    else None -> new StackList[A](List.empty,size - 1)
  def push[B >: A](a: B): StackWithNegativeSize[B] = new StackList(a :: state,size + 1)

  def swap(n: Int): StackWithNegativeSize[A] = {
    val (f, s) = state.splitAt(n)
    StackList(s.head :: f.tail ::: f.head :: s.tail,size)
  }
  def dup(n: Int): StackWithNegativeSize[A] = new StackList(state(n - 1) :: state,size + 1)
}

object StackList {
  def empty[A]: StackWithNegativeSize[A] = new StackList(List.empty[A],0)
  def apply[A](a: A*): StackWithNegativeSize[A] = new StackList(a.toList,a.size)
}
