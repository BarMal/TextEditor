package app

import app.config.AppConfig
import app.config.AppConfig.yamlDecoder
import app.terminal.Term
import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.virtuslab.yaml.*

import scala.language.postfixOps

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private val config: Either[YamlError, AppConfig] =
    scala.io.Source.fromResource("config.yml").mkString.as[AppConfig]

  private def terminal: Resource[IO, Term[IO]] =
    Resource.make(Term.createF[IO])(_.close)

  private def processStream: Term[IO] => IO[ExitCode] = term =>
    term.readStream
      .scan(BufferState.empty)(_ ++ _)
//      .takeThrough(_.exitCondition)
//      .evalTap(term.print)
      .through(state => term._print(state, state.cursorPosition))
      .compile
      .drain
      .as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] =
    terminal
      .use(processStream)
      .handleErrorWith(error =>
        logger
          .error(error)("Something went wrong")
          .as(ExitCode.Error)
      )

}