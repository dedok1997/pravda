package pravda.evm.translate.opcode

import com.google.protobuf.ByteString
import fastparse.byte.all._
import pravda.common.domain.Address
import pravda.evm.EVM._
import pravda.evm.abi.parse.AbiParser
import pravda.evm.{EvmSandbox, readSolidityABI, readSolidityBinFile}
import pravda.evm.abi.parse.AbiParser.AbiFunction
import pravda.evm.debug.evm.{EvmDebugTranslator, EvmDebugger, EvmSandboxDebug}
import pravda.evm.parse.Parser
import pravda.evm.utils._
import pravda.vm.Data
import pravda.vm.asm.PravdaAssembler
import pravda.vm.sandbox.VmSandbox.{ExpectationsWithoutWatts, Preconditions}
import utest._


object RunTests extends TestSuite {

  val tests = Tests {

    val preconditions = Preconditions(`watts-limit` = 10000L)
    val abi = List(AbiFunction(true, "", Nil, Nil, true, "", None))

    "DUP" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x01"), Dup(1)), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x01)), evmWord(Array(0x01)))))

      EvmSandbox.runCode(preconditions, List(Push(hex"0x01"), Push(hex"0x02"), Dup(2)), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x01)), evmWord(Array(0x02)), evmWord(Array(0x01)))))
    }

    "SWAP" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x01"), Push(hex"0x02"), Swap(1)), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x02)), evmWord(Array(0x01)))))

      EvmSandbox.runCode(preconditions, List(Push(hex"0x01"), Push(hex"0x02"), Push(hex"0x03"), Swap(2)), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x03)), evmWord(Array(0x02)), evmWord(Array(0x01)))))
    }

    //TODO tests with overflow

    "ADD" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x01"), Push(hex"0x02"), Add), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x03)))))
    }

    "MUL" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x02"), Push(hex"0x03"), Mul), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x06)))))
    }

    "DIV" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x02"), Push(hex"0x06"), Div), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x03)))))
    }

    "MOD" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x02"), Push(hex"0x05"), Mod), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x01)))))
    }

    "SUB" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x02"), Push(hex"0x05"), Sub), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x03)))))
    }

    "OR" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x05"), Push(hex"0x03"), Or), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x07)))))
    }

    "AND" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x05"), Push(hex"0x03"), And), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x01)))))
    }

    "XOR" - {
      EvmSandbox.runCode(preconditions, List(Push(hex"0x05"), Push(hex"0x03"), Xor), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0x06)))))
    }

//    "BYTE" - {
//      val x = hex"0x426"
//      var pos = hex"0x1f"
//
//      EvmSandbox.runCode(preconditions, List(Push(x), Push(pos), Byte), abi) ==>
//        Right(ExpectationsWithoutWatts(stack = Seq(BigInt(scala.BigInt(x.last.toInt)))))
//
//      pos = hex"0x1e"
//      val expectation = x.reverse.tail.head.toInt
//      EvmSandbox.runCode(preconditions, List(Push(x), Push(pos), Byte), abi) ==>
//        Right(ExpectationsWithoutWatts(stack = Seq(BigInt(scala.BigInt(expectation)))))
//    }

    "LT" - {
      val x = hex"0x4"
      val y = hex"0x2"
      EvmSandbox.runCode(preconditions, List(Push(x), Push(y), Lt), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(1)))))

      EvmSandbox.runCode(preconditions, List(Push(y), Push(x), Lt), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0)))))
    }

    "GT" - {
      val x = hex"0x4"
      val y = hex"0x2"
      EvmSandbox.runCode(preconditions, List(Push(x), Push(y), Gt), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0)))))

      EvmSandbox.runCode(preconditions, List(Push(y), Push(x), Gt), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(1)))))
    }

    "EQ" - {
      val x = hex"0x4"
      val y = hex"0x2"
      EvmSandbox.runCode(preconditions, List(Push(x), Push(y), Eq), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(0)))))

      EvmSandbox.runCode(preconditions, List(Push(x), Push(x), Eq), abi) ==>
        Right(ExpectationsWithoutWatts(stack = Seq(evmWord(Array(1)))))
    }

    "CALL" - {
      // gas addr value argsOffset argsLength retOffset retLength

      //import pravda.evm.function.CodeGenerator._
//      val externalProgram = PravdaAssembler.assemble(
//        pop ~ stop, saveLabels = true)
      val addr1 = ByteString.copyFrom((1 to 20).map(_.toByte).toArray)
      val addr2 = ByteString.copyFrom((2 to 21).map(_.toByte).toArray)

      val addr = Address @@ ByteString.copyFrom(( (1 to 20) ++ (21 to 32 map(_ => 0))).map(_.toByte).toArray)

      //import scala.collection.mutable.ArrayBuffer
      //import pravda.vm.Data.Primitive.{BigInt, Ref}
//      val mem = ArrayBuffer(0 to 100 map(_.toByte) : _*)
//      val preconditions = Preconditions(
//        `watts-limit` = 10000L,
//        stack = Seq(
//          Ref(0),
//          BigInt(scala.BigInt(10)),
//          BigInt(scala.BigInt(20)),
//          BigInt(scala.BigInt(10)),
//          BigInt(scala.BigInt(0)),
//          BigInt(scala.BigInt(1)),
//          Data.Primitive.Bytes(addr1),
//          BigInt(scala.BigInt(1))
//        ),
//        storage = Map(),
//        heap = Map(Ref(0) -> Data.Array.Int8Array(mem)),
//        programs = Map(addr -> Data.Primitive.Bytes(externalProgram))
//      )

//      val pr = SimpleTranslation.evmOpToOps(Call(7)).map(ops => PravdaAssembler.assemble(ops, saveLabels = true))
//
//      pr.map(asmProgram =>
//        VmSandbox.ExpectationsWithoutWatts.fromExpectations(VmSandbox.run(preconditions, asmProgram))
//      )



      "External call" - {



        val Right(ops) = Parser.parseWithIndices(readSolidityBinFile("erc/erc.bin"))
        val Right(abi) = AbiParser.parseAbi(readSolidityABI("erc/erc.abi"))

        val program = EvmDebugTranslator.debugTranslateActualContract(ops,abi).map(ops => PravdaAssembler.assemble(ops, saveLabels = true))
        //println(program)

        val preconditions = Preconditions(`watts-limit` = 10000L,
          stack = Seq(Data.Primitive.Bytes(addr1),Data.Primitive.Bytes(addr2),Data.Primitive.Utf8("transfer")),
          programs = Map(addr -> Data.Primitive.Bytes(program.right.get))
        )

        //println(EvmSandbox.runAddressedCode(preconditions, ops, abi))
        implicit val debugger = EvmDebugger
        implicit val showLog = EvmDebugger.debugLogShow(showStack = true, showHeap = false, showStorage = true)
        implicit val showLogs = EvmDebugger.showDebugLogContainer
        try {
          val Right(output) = EvmSandboxDebug.debugAddressedCode(preconditions, ops, abi)
          println(output)
        }catch {
          case e: Exception =>
        }

        val asmOps = EvmDebugTranslator.debugTranslateActualContract(ops, abi)
        val asmProgramE = asmOps.map(ops => PravdaAssembler.assemble(ops, saveLabels = true))

         for {
          asmProgram <- asmProgramE
          asm = PravdaAssembler.disassemble(asmProgram)
        } {
           import pravda.vm.asm.Operation.mnemonicByOpcode
           import pravda.vm.asm.Operation.Orphan
                      asm.foreach{
             case (q,Orphan(op)) => println(q + ":" + mnemonicByOpcode(op))
             case op => println(op)
           }
         }


      }

    }
  }
}
