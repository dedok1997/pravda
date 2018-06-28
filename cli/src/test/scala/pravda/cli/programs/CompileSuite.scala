package pravda.cli.programs

import cats.Id
import com.google.protobuf.ByteString
import pravda.cli.PravdaConfig
import pravda.cli.PravdaConfig.CompileMode
import pravda.cli.languages.{CompilersLanguage, IoLanguageStub}
import utest._

object CompileSuite extends TestSuite {

  import CompileMode._

  final val UnexpectedBinaryOutput = ByteString.EMPTY
  final val UnexpectedStringOutput = ""

  final val StringSource = "source"
  final val BinarySource = ByteString.copyFromUtf8(StringSource)
  final val ExpectedStringOutput = "expected output"
  final val ExpectedBinaryOutput = ByteString.copyFromUtf8(ExpectedStringOutput)

  val tests: Tests = Tests {
    "asm" - {
      val io = new IoLanguageStub(Some(BinarySource))
      val compilers = new CompilersLanguage[Id] {
        def asm(source: String): Id[Either[String, ByteString]] =
          if (source == StringSource) Right(ExpectedBinaryOutput)
          else Right(UnexpectedBinaryOutput)
        def disasm(source: ByteString): Id[String] =
          UnexpectedStringOutput
        def forth(source: String): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
        def dotnet(source: ByteString): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
      }
      val compile = new Compile[Id](io, compilers)
      compile(PravdaConfig.Compile(Asm))
      assert(io.stdout.headOption.contains(ExpectedBinaryOutput))
    }
    "disasm" - {
      val io = new IoLanguageStub(Some(BinarySource))
      val compilers = new CompilersLanguage[Id] {
        def asm(source: String): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
        def disasm(source: ByteString): Id[String] =
          if (source == BinarySource) ExpectedStringOutput
          else UnexpectedStringOutput
        def forth(source: String): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
        def dotnet(source: ByteString): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
      }
      val compile = new Compile[Id](io, compilers)
      compile(PravdaConfig.Compile(Disasm))
      assert(io.stdout.headOption.contains(ExpectedBinaryOutput))
    }
    "forth" - {
      val io = new IoLanguageStub(Some(BinarySource))
      val compilers = new CompilersLanguage[Id] {
        def asm(source: String): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
        def disasm(source: ByteString): Id[String] =
          UnexpectedStringOutput
        def forth(source: String): Id[Either[String, ByteString]] =
          if (source == StringSource) Right(ExpectedBinaryOutput)
          else Right(UnexpectedBinaryOutput)
        def dotnet(source: ByteString): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
      }
      val compile = new Compile[Id](io, compilers)
      compile(PravdaConfig.Compile(Forth))
      assert(io.stdout.headOption.contains(ExpectedBinaryOutput))
    }

    "dotnet" - {
      val io = new IoLanguageStub(Some(BinarySource))
      val compilers = new CompilersLanguage[Id] {
        def asm(source: String): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
        def disasm(source: ByteString): Id[String] =
          UnexpectedStringOutput
        def forth(source: String): Id[Either[String, ByteString]] =
          Right(UnexpectedBinaryOutput)
        def dotnet(source: ByteString): Id[Either[String, ByteString]] =
          if (source == BinarySource) Right(ExpectedBinaryOutput)
          else Right(UnexpectedBinaryOutput)
      }
      val compile = new Compile[Id](io, compilers)
      compile(PravdaConfig.Compile(DotNet))
      assert(io.stdout.headOption.contains(ExpectedBinaryOutput))
    }
  }
}
