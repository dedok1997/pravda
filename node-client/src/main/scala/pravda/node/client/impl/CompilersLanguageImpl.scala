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

package pravda.node.client.impl

import com.google.protobuf.ByteString
import pravda.dotnet.parser.FileParser
import pravda.dotnet.translation.{Translator => DotnetTranslator}
import pravda.vm.asm.{Operation, PravdaAssembler}
import cats.implicits._
import pravda.evm.debug.evm.{EvmDebugger, EvmSandboxDebug}
import pravda.node.client.CompilersLanguage
import pravda.yaml4s
import pravda.vm.Data
import pravda.vm.sandbox.VmSandbox.Preconditions

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final class CompilersLanguageImpl(implicit executionContext: ExecutionContext) extends CompilersLanguage[Future] {

  def asm(fileName: String, source: String): Future[Either[String, ByteString]] = Future {
    PravdaAssembler.assemble(source, saveLabels = true).left.map(s => s"$fileName:${s.mkString}")
  }

  def asm(source: String): Future[Either[String, ByteString]] = Future {
    PravdaAssembler.assemble(source, saveLabels = true).left.map(_.mkString)
  }

  def disasm(source: ByteString): Future[String] = Future {
    PravdaAssembler.render(PravdaAssembler.disassemble(source).map(_._2))
  }

  def disasmToOps(source: ByteString): Future[Seq[(Int, Operation)]] = Future {
    PravdaAssembler.disassemble(source)
  }

  def dotnet(sources: Seq[(ByteString, Option[ByteString])],
             mainClass: Option[String]): Future[Either[String, ByteString]] = Future {
    for {
      files <- sources
        .map {
          case (pe, pdb) => FileParser.parseDotnetFile(pe.toByteArray, pdb.map(_.toByteArray))
        }
        .toList
        .sequence
      ops <- DotnetTranslator.translateAsm(files, mainClass).left.map(_.mkString)
    } yield PravdaAssembler.assemble(ops, saveLabels = true)
  }

  def evm(sourceBytes: ByteString, abiBytes: ByteString): Future[Either[String, ByteString]] = Future {
    import pravda.evm.abi.parse.AbiParser._
    import pravda.evm.parse.Parser._
    import pravda.evm.translate.Translator._
    import pravda.evm.utils.readBin

    val source = readBin(sourceBytes.toStringUtf8)
    val abiS = abiBytes.toStringUtf8

    for {
      abi <- parseAbi(abiS)
      ops <- parseWithIndices(source)
      asmOps <- translateActualContract(ops, abi)
    } yield PravdaAssembler.assemble(asmOps, saveLabels = true)
  }

  def evmTrace(sourceBytes: ByteString,
               abiBytes: ByteString,
               yamlBytes: ByteString): Future[Either[String, ByteString]] = Future {
    import pravda.evm.abi.parse.AbiParser._
    import pravda.evm.parse.Parser._
    import pravda.evm.utils.readBin

    import com.google.protobuf.ByteString
    import org.json4s.DefaultFormats
    import pravda.common.json._
    import pravda.vm
    import pravda.vm.Data.Primitive
    import pravda.vm.json._

    implicit val debugger = EvmDebugger
    implicit val showLog = EvmDebugger.debugLogShow(showStack = true, showHeap = false, showStorage = true)
    implicit val showLogs = EvmDebugger.showDebugLogContainer

    implicit val formats =
      DefaultFormats +
        json4sFormat[Data] +
        json4sFormat[Primitive] +
        json4sFormat[Primitive.Int64] +
        json4sFormat[Primitive.Bytes] +
        json4sFormat[ByteString] +
        json4sFormat[vm.Effect] +
        json4sFormat[vm.Error] +
        json4sKeyFormat[ByteString] +
        json4sKeyFormat[Primitive.Ref] +
        json4sKeyFormat[Primitive]

    val yaml = yamlBytes.toStringUtf8

    for {
      yamlL <- yaml4s.parseAllYamlOpt(yaml, useBigDecimalForDouble = false).toRight("Couldn't parse yaml file")
      preconditions <- yamlL.headOption.toRight("Incorrect format")
      precondition <- Try { preconditions.extract[Preconditions] }.toEither.left.map(_.toString)
      source = readBin(sourceBytes.toStringUtf8)
      abiS = abiBytes.toStringUtf8
      abi <- parseAbi(abiS)
      ops <- parseWithIndices(source)
      output <- EvmSandboxDebug.debugAddressedCode(precondition, ops, abi)
      result = ByteString.copyFromUtf8(output)
    } yield result
  }
}
