package app.terminal

import cats.Applicative
import cats.effect.kernel.Async
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.Terminal
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import cats.implicits.toFunctorOps

class Reader[F[_]: Async](terminal: Terminal) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private def safeRead: F[Option[KeyStroke]] =
    try Async[F].blocking(terminal.readInput()).map(Option(_))
    catch
      case ex: Exception =>
        logger.error(ex)(s"""Error reading input ${ex.getMessage}""").as(None)

  def readStream: fs2.Stream[F, KeyStroke] =
    fs2.Stream
      .repeatEval(safeRead)
      .collect { case Some(value) =>
        value
      }
}
