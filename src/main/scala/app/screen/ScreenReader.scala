package app.screen

import cats.{Applicative, MonadError}
import cats.effect.kernel.Async
import cats.implicits.{catsSyntaxApplyOps, toFlatMapOps, toFunctorOps}
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.screen.Screen
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.util.{Failure, Success, Try}

class ScreenReader[F[_]: Async](screen: Screen) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private def safeRead: F[Option[KeyStroke]] =
    Async[F].blocking(Try(Option(screen.readInput()))).flatMap {
      case Success(value) => Applicative[F].pure(value)
      case Failure(exception) =>
        logger
          .error(exception)(
            s"""Error reading input ${exception.getMessage}"""
          ) *> MonadError[F, Throwable]
          .raiseError(exception)
    }

  def readStream: fs2.Stream[F, KeyStroke] =
    fs2.Stream
      .repeatEval(safeRead)
      .collect { case Some(value) => value }

}
