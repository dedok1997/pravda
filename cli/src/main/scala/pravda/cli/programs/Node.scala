package pravda.cli.programs

import cats._
import cats.data.EitherT
import cats.implicits._
import com.google.protobuf.ByteString
import pravda.cli.Config
import pravda.cli.Config.Node.{Mode, Network}
import pravda.cli.languages.{IoLanguage, NodeLanguage, RandomLanguage}
import pravda.common.{bytes, crypto}
import pravda.node.data.TimechainConfig.PaymentWallet
import pravda.node.data.common.Address
import pravda.node.data.cryptography.PrivateKey

import scala.language.higherKinds

final class Node[F[_]: Monad](io: IoLanguage[F], random: RandomLanguage[F], node: NodeLanguage[F]) {

  private def applicationConfig(isValidator: Boolean,
                                chainId: String,
                                dataDir: String,
                                paymentWallet: PaymentWallet,
                                validators: Seq[String],
                                seeds: Seq[(String, Int)]) =
    s"""timechain {
       |  api {
       |    host = "127.0.0.1"
       |    port = 8080
       |  }
       |  tendermint {
       |    peer-port = 46656
       |    rpc-port = 46657
       |    proxy-app-port = 46658
       |    use-unix-domain-socket = false
       |  }
       |  is-validator = $isValidator
       |  data-directory = "$dataDir"
       |  seeds = "${seeds.map { case (host, port) => s"$host:$port" }.mkString(",")}"
       |  genesis {
       |    time = "0001-01-01T00:00:00Z"
       |    chain-id = "$chainId"
       |    validators = "${validators.mkString(",")}"
       |    app-hash = ""
       |    distribution = true
       |  }
       |  payment-wallet {
       |    private-key = "${bytes.byteString2hex(paymentWallet.privateKey)}"
       |    address = "${bytes.byteString2hex(paymentWallet.address)}"
       |  }
       |}
       |""".stripMargin

  private def mkConfigPath(dataDir: String): F[String] =
    io.concatPath(dataDir, "node.conf")

  private def init(dataDir: String, network: Network): F[Unit] = {
    for {
      configPath <- io.concatPath(dataDir, "node.conf")
      randomBytes <- random.secureBytes64()
      (pub, sec) = crypto.ed25519KeyPair(randomBytes)
      paymentWallet = PaymentWallet(PrivateKey @@ sec, Address @@ pub)
      config = network match {
        case Network.Local =>
          applicationConfig(
            isValidator = true,
            chainId = "local",
            dataDir = dataDir,
            paymentWallet = paymentWallet,
            validators = List(s"me:10:${bytes.byteString2hex(pub)}"),
            seeds = Nil
          )
        case Network.Testnet =>
          applicationConfig(isValidator = false, "testnet", dataDir, paymentWallet, Nil, Nil)
      }
      _ <- io.writeToFile(configPath, ByteString.copyFromUtf8(config))
    } yield ()
  }

  def apply(config: Config.Node): F[Unit] = {
    val errorOrOk =
      for {
        dataDir <- EitherT.liftF {
          config.dataDir.map(Monad[F].pure).getOrElse {
            for {
              pwd <- io.pwd()
              dataDir <- io.concatPath(pwd, "pravda-data")
              _ <- io.mkdirs(dataDir)
            } yield dataDir
          }
        }
        _ <- EitherT[F, String, Unit] {
          io.isDirectory(dataDir).flatMap {
            case None        => io.mkdirs(dataDir).map(Right.apply)
            case Some(true)  => Monad[F].pure(Right(()))
            case Some(false) => Monad[F].pure(Left(s"'$dataDir' is not a directory."))
          }
        }
        _ <- EitherT[F, String, Unit] {
          config.mode match {
            case Mode.Nope          => Monad[F].pure(Left(s"[init|run] subcommand required."))
            case Mode.Init(network) => init(dataDir, network).map(Right.apply)
            case Mode.Run           => mkConfigPath(dataDir).flatMap(node.launch).map(Right.apply)
          }
        }
      } yield ()

    errorOrOk.value.flatMap {
      case Left(error) => io.writeStringToStderrAndExit(s"$error\n")
      case Right(_)    => Monad[F].unit
    }
  }
}
