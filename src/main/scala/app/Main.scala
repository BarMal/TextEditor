package app

import app.config.AppConfig
import app.config.AppConfig.yamlDecoder
import app.render.Renderer
import app.terminal.{Reader, Term, Writer}
import cats.effect.*
import com.googlecode.lanterna.terminal.Terminal
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.virtuslab.yaml.*

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private val config: Either[YamlError, AppConfig] =
    scala.io.Source.fromResource("config.yml").mkString.as[AppConfig]

  private def terminalRes: Resource[IO, Terminal] =
    Resource.make(Term.createF[IO])(Term.closeTerminalF[IO])

  private val bufferStateRef: IO[Ref[IO, BufferState]] =
    Ref.of[IO, BufferState](BufferState.empty)

  private def out: Writer[IO] => Ref[IO, BufferState] => fs2.Stream[IO, Unit] =
    writer =>
      state =>
        fs2.Stream
          .constant(System.currentTimeMillis())
          .metered[IO](16 milliseconds)
          .evalTap(startTime =>
            state.get.flatMap(Renderer.render(writer, startTime, _))
          )
          .void

  private def in: Reader[IO] => Ref[IO, BufferState] => fs2.Stream[IO, Unit] =
    reader =>
      state =>
        reader.readStream
          .evalTap(keyStroke => state.update(_ ++ keyStroke))
          .void

  private def process: Terminal => Ref[IO, BufferState] => IO[ExitCode] =
    term =>
      stateRef =>
        out(new Writer[IO](term))(stateRef)
          .concurrently(in(new Reader[IO](term))(stateRef))
          .compile
          .drain
          .as(ExitCode.Success)
          .handleErrorWith(
            logger
              .error(_)("Something went wrong with the stream")
              .as(ExitCode.Error)
          )

  override def run(args: List[String]): IO[ExitCode] =
    terminalRes
      .use { term =>
        for {
          state   <- bufferStateRef
          program <- process(term)(state)
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
