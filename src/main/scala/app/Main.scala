package app

import app.config.AppConfig
import app.config.AppConfig.yamlDecoder
import app.terminal.Term
import cats.effect.*
import com.googlecode.lanterna.input.KeyStroke
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.virtuslab.yaml.*

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private val config: Either[YamlError, AppConfig] =
    scala.io.Source.fromResource("config.yml").mkString.as[AppConfig]

  private def terminal: Resource[IO, Term[IO]] =
    Resource.make(Term.createF[IO])(_.close)

  private val cursorBlinkInterval: FiniteDuration = 500 milliseconds

  private val bufferStateRef: IO[Ref[IO, BufferState]] =
    Ref.of[IO, BufferState](BufferState.empty)

  private def in(
      term: Term[IO],
      state: Ref[IO, BufferState]
  ): fs2.Stream[IO, KeyStroke] =
    term.readStream
      .evalTap(keyStroke => state.update(_ ++ keyStroke))

  private def render(term: Term[IO], state: Ref[IO, BufferState]) =
    fs2.Stream
      .constant(System.currentTimeMillis())
      .evalTap { startTime =>
        val elapsedTime = System.currentTimeMillis() - startTime
        val shouldBlink: Boolean = Math.floorDiv(
          elapsedTime,
          cursorBlinkInterval.toMillis
        ) % 2 == 0

        for {
          state <- state.get
          _     <- term.print(state, shouldBlink)
        } yield ()
      }

  private def newProcess: Term[IO] => Ref[IO, BufferState] => IO[ExitCode] =
    term =>
      state =>
        render(term, state)
          .concurrently(in(term, state))
          .compile
          .drain
          .as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] =
    for {
      state <- bufferStateRef
      program <- terminal
        .use(newProcess(_)(state))
        .handleErrorWith(
          logger
            .error(_)("Something went wrong")
            .as(ExitCode.Error)
        )
    } yield program

}
