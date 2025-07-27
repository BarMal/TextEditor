package app

import app.buffer.BufferState
import app.config.AppConfig
import app.render.Renderer
import app.screen.{ScreenReader, ScreenWriter}
import cats.effect.*
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private val config: Result[AppConfig] =
    ConfigSource.default.load[AppConfig]

  private def screenRes: Resource[IO, Screen] =
    Resource.make {
      Try {
        val term: Terminal = DefaultTerminalFactory()
          .setTerminalEmulatorTitle("Bam")
          .setInitialTerminalSize(new TerminalSize(100, 50))
          .createTerminal()
        new TerminalScreen(term)
      } match {
        case Failure(exception) =>
          logger.error(exception)("Failed to start screen resource") *>
            IO.raiseError(exception)
        case Success(screen) => logger.info("Created screen").as(screen)
      }
    } { screen =>
      Try(screen.stopScreen()) match {
        case Failure(exception) =>
          logger.error(exception)("Failed to release screen resource") *>
            IO.raiseError(exception)
        case Success(_) => logger.info("Stopped screen").void
      }
    }

  private val bufferStateRef: IO[Ref[IO, BufferState]] =
    Ref.of[IO, BufferState](BufferState.empty)

  private val lastBufferStateHashRef: IO[Ref[IO, Option[Int]]] =
    Ref.of[IO, Option[Int]](None)

  private def in(
      reader: ScreenReader[IO],
      stateRef: Ref[IO, BufferState],
      lastHashRef: Ref[IO, Option[Int]]
  ): fs2.Stream[IO, Unit] =
    reader.readStream
      .evalMap { keyStroke =>
        for {
          lastState <- stateRef.getAndUpdate(_ ++ keyStroke)
          _         <- lastHashRef.update(_ => Some(lastState.hashCode()))
        } yield ()
      }

  private def out(
      writer: ScreenWriter[IO],
      stateRef: Ref[IO, BufferState],
      lastStateRef: Ref[IO, Option[Int]]
  ): fs2.Stream[IO, Unit] =
    fs2.Stream
      .constant(System.currentTimeMillis())
      .metered[IO](16 milliseconds)
      .evalMap(* =>
        for {
          state         <- stateRef.get
          lastStateHash <- lastStateRef.get
          _ <- IO.unlessA(lastStateHash.contains(state.hashCode()))(
            Renderer.render(writer, state)
          )
        } yield ()
      )

  private def process(
      screen: Screen,
      stateRef: Ref[IO, BufferState],
      lastHashState: Ref[IO, Option[Int]]
  ): IO[ExitCode] =
    out(new ScreenWriter[IO](screen), stateRef, lastHashState)
      .concurrently(in(new ScreenReader[IO](screen), stateRef, lastHashState))
      .compile
      .drain
      .as(ExitCode.Success)
      .handleErrorWith(
        logger
          .error(_)("Something went wrong with the stream")
          .as(ExitCode.Error)
      )

  override def run(args: List[String]): IO[ExitCode] =
    screenRes
      .use { screen =>
        for {
          state         <- bufferStateRef
          lastHashState <- lastBufferStateHashRef
          _ <- Try(screen.startScreen()) match {
            case Failure(exception) =>
              logger.error(exception)("Couldn't start screen")
                *> IO.raiseError(exception)
            case Success(_) => IO.unit
          }
          program <- process(screen, state, lastHashState)
        } yield program
      }
      .handleErrorWith(
        logger
          .error(_)("Something went wrong with the resource")
          .as(ExitCode.Error)
      )

  extension [F[_]: Async](stream: fs2.Stream[F, ?]) {
    def void: fs2.Stream[F, Unit] = stream.as(())
  }

}
