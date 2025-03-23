package app

import app.config.AppConfig
import app.config.AppConfig.yamlDecoder
import app.terminal.Term
import cats.effect.{ExitCode, IO, IOApp, Ref, Resource}
import fs2.Chunk
import fs2.Chunk.Queue
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.virtuslab.yaml.*

import scala.language.postfixOps
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import cats.syntax.parallel.catsSyntaxTuple2Parallel
import cats.effect.implicits.parallelForGenSpawn
import com.googlecode.lanterna.input.KeyStroke

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private val config: Either[YamlError, AppConfig] =
    scala.io.Source.fromResource("config.yml").mkString.as[AppConfig]

  private def terminal: Resource[IO, Term[IO]] =
    Resource.make(Term.createF[IO])(_.close)

  private val stateRef: IO[Ref[IO, BufferState]] =
    Ref.of[IO, BufferState](BufferState.empty)

  private val cursorBlinkInterval: FiniteDuration = 500 milliseconds

  private def in(term: Term[IO]): fs2.Stream[IO, KeyStroke] =
    term.readStream
      .evalTap(keyStroke =>
        stateRef.flatMap(ref => ref.getAndUpdate(_ ++ keyStroke))
      )

  private def render(term: Term[IO]) =
    fs2.Stream
      .constant(System.currentTimeMillis())
      .evalTap { startTime =>
        val elapsedTime = System.currentTimeMillis() - startTime
        val shouldBlink: Boolean = Math.floorDiv(
          elapsedTime,
          cursorBlinkInterval.toMillis
        ) % 2 == 0

        for {
          ref   <- stateRef
          state <- ref.get
          _ <- term.print(
            state,
            (
              Math.floorMod(state.cursorPosition, state.lineLength),
              Math.floorDiv(state.cursorPosition, state.lineLength)
            ),
            shouldBlink
          )
        } yield ()
      }

  private def newProcess: Term[IO] => IO[ExitCode] = term =>
    render(term).concurrently(in(term)).compile.drain.as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] =
    terminal
      .use(newProcess)
      .handleErrorWith(error =>
        logger
          .error(error)("Something went wrong")
          .as(ExitCode.Error)
      )

}
