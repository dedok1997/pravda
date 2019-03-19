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

import java.nio.ByteBuffer

import cats.Show
import pravda.evm.debug.DebugVm.{ExecutionResult, InterruptedExecution, MetaExecution, UnitExecution}
import pravda.evm.debug.Debugger
import pravda.vm.Meta._
import pravda.vm.impl.MemoryImpl
import pravda.vm._
import pravda.vm.asm.Operation.mnemonicByOpcode
import pravda.vm.sandbox.VmSandbox.StorageSandbox

import scala.annotation.tailrec

sealed trait DebugLog

final case class EvmOpLog(op: String)                                                        extends DebugLog
final case class PravdaOpLog(op: String, snapshot: MemorySnapshot, storage: StorageSnapshot) extends DebugLog
final case class ErrorLog(op: String, snapshot: MemorySnapshot, storage: StorageSnapshot)    extends DebugLog

final case class MemorySnapshot(stack: List[Data.Primitive], heap: List[Data])
final case class StorageSnapshot(items: Map[Data.Primitive, Data])

object EvmDebugger extends Debugger[DebugLog] {
  override def debugOp(program: ByteBuffer, op: Int, mem: MemoryImpl, storage: StorageSandbox)(
      exec: Either[Throwable, ExecutionResult]): DebugLog = {
    val memorySnap = MemorySnapshot(mem.stack.toList, mem.heap.toList)
    val storageSnap = StorageSnapshot(storage.storageItems.toMap)
    val log = exec match {
      case Right(UnitExecution(_)) =>
        PravdaOpLog(mnemonicByOpcode(op), memorySnap, storageSnap)
      case Right(ex @ MetaExecution(Custom(s))) if s.startsWith(EvmDebugTranslator.debugMarker) =>
        EvmOpLog(s.stripPrefix(EvmDebugTranslator.debugMarker))
      case Right(ex @ MetaExecution(l)) =>
        PravdaOpLog(l.toString, memorySnap, storageSnap)
      case Right(InterruptedExecution) =>
        PravdaOpLog(mnemonicByOpcode(op), memorySnap, storageSnap)
      case Left(e: Data.DataException) =>
        ErrorLog(s"${mnemonicByOpcode(op)} - ${e.toString}", memorySnap, storageSnap)
      case Left(ThrowableVmError(e)) =>
        ErrorLog(s"${mnemonicByOpcode(op)} - ${e.toString}", memorySnap, storageSnap)
      case Left(e : Exception) =>
        ErrorLog(s"${mnemonicByOpcode(op)} - ${e.toString}", memorySnap, storageSnap)
    }

    println(debugLogShow(true,false,false).show(log))
    log
  }


   import pravda.vm.operations._
   override def prettifier(logs: List[DebugLog]): List[DebugLog] = {
     @tailrec def aux(logs: List[DebugLog],acc: List[DebugLog] = Nil):List[DebugLog] =
     logs match {
       case Nil => acc.reverse
       case PravdaOpLog("push", m, _) :: PravdaOpLog("dupn", mem, storage) :: xs =>
         val offs = integer(m.stack.reverse.head)
         aux(xs, PravdaOpLog(s"dup($offs)", mem, storage) :: acc)
       case PravdaOpLog("push", m, _) :: PravdaOpLog("swapn", mem, storage) :: xs =>
         val offs = integer(m.stack.reverse.head) - 1
         aux(xs, PravdaOpLog(s"swap($offs)", mem, storage) :: acc)

       case PravdaOpLog("push", _, _) :: PravdaOpLog("cast", mem, storage) :: xs =>
         val offs = mem.stack.reverse.head
         aux(xs, PravdaOpLog(s"cast to $offs", mem, storage) :: acc)
       case PravdaOpLog("dup", mem, storage) :: xs =>
         aux(xs, PravdaOpLog(s"dup(1)", mem, storage) :: acc)

       case PravdaOpLog("swap", mem, storage) :: xs =>
         aux(xs, PravdaOpLog(s"swap(1)", mem, storage) :: acc)

       case PravdaOpLog("pop", _, _) ::
            PravdaOpLog(lbl, _, _) ::
            PravdaOpLog("push", m, _) ::
            PravdaOpLog("jump", mem, storage) :: xs =>
         val offs = offset(m.stack.reverse.head)
         aux(xs, PravdaOpLog(s"Jump to $lbl in $offs", mem, storage) :: acc)

       case PravdaOpLog(lbl, _, _) ::
            PravdaOpLog("push", m, _) ::
            PravdaOpLog("jumpi", mem, storage) :: xs =>
         val offs = offset(m.stack.reverse.head)
         val cond = boolean(m.stack.reverse.tail.head)

         aux(xs, PravdaOpLog(s"JumpI($cond) to $lbl in $offs", mem, storage) :: acc)



       case x :: xs => aux(xs, x :: acc)
     }
     aux(logs)
   }

  def debugLogShow(showStack: Boolean, showHeap: Boolean, showStorage: Boolean): cats.Show[DebugLog] = { log =>
    def t(count: Int)(s: String) = "\t" * count + s

    log match {
      case EvmOpLog(s) =>
        s
      case PravdaOpLog(op, memSnap, storSnap) =>
        (s"\t\t$op" ::
          (if (showStack)
           t(3)(s"stack size = ${memSnap.stack.size}:") :: memSnap.stack.reverse.map(el => t(4)(el.toString))
         else Nil) :::
          (if (showHeap) t(3)(s"heap size = ${memSnap.heap.size}:") :: memSnap.heap.map(el => t(4)(el.toString))
         else Nil)) :::
          (if (showStorage)
           t(3)(s"storage size = ${storSnap.items.size}:") :: storSnap.items
             .map(el => t(4)(s"${el._1} -> ${el._2}"))
             .toList
         else Nil) mkString "\n"
      case ErrorLog(msg, memSnap, storSnap) =>
        (s"\t\t$msg" ::
          (if (showStack)
           t(3)(s"stack size = ${memSnap.stack.size}:") :: memSnap.stack.reverse.map(el => t(4)(el.toString))
         else Nil) :::
          (if (showHeap) t(3)(s"heap size = ${memSnap.heap.size}:") :: memSnap.heap.map(el => t(4)(el.toString))
         else Nil)) :::
          (if (showStorage)
           t(3)(s"storage size = ${storSnap.items.size}:") :: storSnap.items
             .map(el => t(4)(s"${el._1} -> ${el._2}"))
             .toList
         else Nil) mkString "\n"
    }
  }

  def showDebugLogContainer(implicit showDebugLog: Show[DebugLog]): cats.Show[List[DebugLog]] =
    _.map(showDebugLog.show).mkString("\n")

}