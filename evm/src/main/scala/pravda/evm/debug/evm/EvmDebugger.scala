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

import cats.{Foldable, Show}
import pravda.evm.debug.DebugVm.{ExecutionResult, InterruptedExecution, MetaExecution, UnitExecution}
import pravda.evm.debug.Debugger
import pravda.vm.Meta._
import pravda.vm.impl.MemoryImpl
import pravda.vm._
import pravda.vm.asm.Operation.mnemonicByOpcode
import pravda.vm.sandbox.VmSandbox.StorageSandbox

import scala.util.{Failure, Success, Try}

sealed trait DebugLog

final case class EvmOpLog(op: String)                                                        extends DebugLog
final case class PravdaOpLog(op: String, snapshot: MemorySnapshot, storage: StorageSnapshot) extends DebugLog
final case class ErrorLog(op: String, snapshot: MemorySnapshot, storage: StorageSnapshot)    extends DebugLog

final case class MemorySnapshot(stack: List[Data.Primitive], heap: List[Data])
final case class StorageSnapshot(items: Map[Data.Primitive, Data])

object EvmDebugger extends Debugger[DebugLog] {
  override def debugOp(program: ByteBuffer, op: Int, mem: MemoryImpl, storage: StorageSandbox)(
      exec: Try[ExecutionResult]): DebugLog = {
    val memorySnap = MemorySnapshot(mem.stack.toList, mem.heap.toList)
    val storageSnap = StorageSnapshot(storage.storageItems.toMap)
    exec match {
      case Success(UnitExecution(_)) =>
        PravdaOpLog(mnemonicByOpcode(op), memorySnap, storageSnap)
      case Success(ex @ MetaExecution(Custom(s))) if s.startsWith(EvmDebugTranslator.debugMarker) =>
        EvmOpLog(s.stripPrefix(EvmDebugTranslator.debugMarker))
      case Success(ex @ MetaExecution(l)) =>
        PravdaOpLog(l.toString, memorySnap, storageSnap)
      case Success(InterruptedExecution) =>
        PravdaOpLog(mnemonicByOpcode(op), memorySnap, storageSnap)
      case Failure(e: Data.DataException) =>
        ErrorLog(s"${mnemonicByOpcode(op)} - ${e.toString}", memorySnap, storageSnap)
      case Failure(ThrowableVmError(e)) =>
        ErrorLog(s"${mnemonicByOpcode(op)} - ${e.toString}", memorySnap, storageSnap)
    }
  }

  private def t(count: Int)(s: String) = "\t" * count + s

  def DebugLogShow(showStack: Boolean, showHeap: Boolean, showStorage: Boolean): cats.Show[DebugLog] = {
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

  import scala.language.higherKinds

  def showDebugLogContainer[F[_]: Foldable](implicit showDebugLog: Show[DebugLog]): cats.Show[F[DebugLog]] =
    t =>
      implicitly[Foldable[F]]
        .foldLeft(t, "") {
          case (acc, log) =>
            acc + s"${showDebugLog.show(log)}\n"
      }

}
