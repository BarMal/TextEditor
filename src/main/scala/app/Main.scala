package app

import app.State.showInstance
import app.config.AppConfig
import app.config.AppConfig.yamlDecoder
import app.terminal.Term
import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.virtuslab.yaml.*

import scala.language.postfixOps

object Main extends IOApp {

  private val config: Either[YamlError, AppConfig] =
    scala.io.Source.fromResource("config.yml").mkString.as[AppConfig]

  private def terminal: Resource[IO, Term[IO]] =
    Resource.make(Term.createF[IO])(_.close)

  private def processStream: Term[IO] => IO[ExitCode] = term =>
    term.readStream
      .scan(State.empty)(_ ++ _)
      .takeThrough(_.exitCondition)
      .evalTap(term.print(_))
      .compile
      .drain
      .as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] =
    terminal.use(processStream).handleError(_ => ExitCode.Error)

}
