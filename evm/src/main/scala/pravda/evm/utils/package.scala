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

package pravda.evm

import com.google.protobuf.ByteString
import pravda.evm.EVM._
import pravda.evm.UtilsData.Info
import pravda.vm.Data

package object utils {

  def evmWord(arr: Array[Byte]): Data.Primitive.Bytes =
    Data.Primitive.Bytes(ByteString.copyFrom(arr).concat(ByteString.copyFrom(Array.fill[Byte](32 - arr.length)(0))))

  def readBin(s: String): Array[Byte] = s.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte).dropRight(43)



  def info(ops: List[Op]): Info = ops.foldLeft(Info(0,0,0)){
    case (acc,SelfAddressedJump(_) | Jump | Jump(_,_)) => acc.copy(jumpCount = acc.jumpCount + 1)
    case (acc,SelfAddressedJumpI(_) | JumpI | JumpI(_,_)) => acc.copy(jumpiCount = acc.jumpiCount + 1)
    case (acc, JumpDest(_) | JumpDest) => acc.copy(jumpdestCount = acc.jumpdestCount + 1)
    case (acc,_) => acc
  }
}

object UtilsData
{
  case class Info(jumpCount: Int,jumpiCount: Int,jumpdestCount: Int)

}

