package app.terminal

import cats.Applicative
import cats.effect.kernel.Async
import cats.implicits.{toFlatMapOps, toFunctorOps}
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.Terminal
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.util.{Failure, Success, Try}

class Reader[F[_]: Async](terminal: Terminal) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private def safeRead: F[Option[KeyStroke]] =
    Async[F].blocking(Try(Option(terminal.readInput()))).flatMap {
      case Success(value) => Applicative[F].pure(value)
      case Failure(exception) =>
        logger
          .error(exception)(s"""Error reading input ${exception.getMessage}""")
          .as(None)
    }

  def readStream: fs2.Stream[F, KeyStroke] =
    fs2.Stream
      .repeatEval(safeRead)
      .collect { case Some(value) => value }
}
