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

  private def in(term: Term[IO]): fs2.Stream[IO, BufferState] =
    term.readStream
      .scan(BufferState.empty)(_ ++ _)
      .evalTap(state => stateRef.map(ref => ref.update(_ => state)))

  private def render(term: Term[IO]) =
    fs2.Stream
      .constant(System.currentTimeMillis())
      .evalTap { startTime =>
        val elapsedTime = System.currentTimeMillis() - startTime
        val shouldBlink: Boolean = Math.floorDiv(
          elapsedTime,
          cursorBlinkInterval.toMillis
        ) % 2 == 0

        logger.info(s"""Rendering with start $startTime, elapsed $elapsedTime""")

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

  private def processStream: Term[IO] => IO[ExitCode] = term =>
    term.readStream
      .scan(BufferState.empty)(_ ++ _)
      .parEvalMap(1) { state =>
        term.print(
          state,
          (
            Math.floorMod(state.cursorPosition, state.lineLength),
            Math.floorDiv(state.cursorPosition, state.lineLength)
          ),
          false
        )
      }
      .compile
      .drain
      .as(ExitCode.Success)
//      .evalTap { state =>
//        print(
//          state,
//          (
//            Math.floorMod(state.cursorPosition, state.lineLength),
//            Math.floorDiv(state.cursorPosition, state.lineLength)
//          ),
//          Math.floorDiv(
//            System.currentTimeMillis() - startTime,
//            cursorBlinkInterval.toMillis
//          ) % 2 == 0
//        )
//      }
////      .takeThrough(_.exitCondition)
//      .compile
//      .drain
//      .as(ExitCode.Success)

  // Shared state: BufferState (text editor contents) + cursor visibility
//  Ref.of[IO, (BufferState, Boolean)]((BufferState.empty, false)).flatMap { stateRef =>
//
//    // Input Stream: Updates BufferState in Ref
//    val inputStream: Term[IO] => fs2.Stream[IO, BufferState] = term => term.readStream
//      .scan(BufferState.empty)(_ ++ _)
//      .evalTap(newState => stateRef.update { case (_, cursorVisible) => (newState, cursorVisible) })
//
//    // Cursor Blink Stream: Toggles cursor visibility in Ref every 500ms
//    val cursorBlinkStream = fs2.Stream
//      .awakeEvery[IO](500.millis)
//      .mapAccumulate(false)((visible, _) => (!visible, !visible))
//      .map(_._2)
//      .evalMap(cursorVisible => stateRef.update { case (state, _) => (state, cursorVisible) })
//
//    // Render Stream: Periodically reads Ref & updates terminal
//    val renderStream: Term[IO] => fs2.Stream[IO, (BufferState, Boolean)] = term => fs2.Stream
//      .awakeEvery[IO](16.millis) // ~60FPS rendering
//      .evalMap(_ => stateRef.get)
//      .evalTap { case (state, cursorVisible) =>
//        term.print(
//          state,
//          (
//            Math.floorMod(state.cursorPosition, state.lineLength),
//            Math.floorDiv(state.cursorPosition, state.lineLength)
//          ),
//          cursorVisible
//        )
//      }
//
//    // Run all streams concurrently
//    (inputStream, cursorBlinkStream).parTupled *> renderStream.as(ExitCode.Success)
//  }

  private def newProcess: Term[IO] => IO[ExitCode] = term =>
    render(term).evalTap(_ => in(term).compile.drain).compile.drain.as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] =
    terminal
      .use(newProcess)
      .handleErrorWith(error =>
        logger
          .error(error)("Something went wrong")
          .as(ExitCode.Error)
      )

}
