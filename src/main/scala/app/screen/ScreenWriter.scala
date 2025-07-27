package app.screen

import app.render.Output
import cats.effect.kernel.Async
import cats.implicits.{
  catsSyntaxApplyOps,
  toFlatMapOps,
  toFunctorOps,
  toTraverseOps
}
import cats.{Monad, MonadError}
import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.screen.Screen
import com.googlecode.lanterna.screen.Screen.RefreshType
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.util.{Failure, Success, Try}

class ScreenWriter[F[_]: Async](screen: Screen) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  def updateCursorPosition(x: Int, y: Int): F[Unit] =
    Async[F].blocking(screen.setCursorPosition(new TerminalPosition(x, y)))

  def print(elements: Vector[Output]): F[Unit] =
    Async[F].blocking(screen.clear()) *>
      elements
        .traverse(elem =>
          Async[F]
            .blocking(screen.setCharacter(elem.x, elem.y, elem.textCharacter))
        )
        .flatTap { _ =>
          Try(screen.refresh(RefreshType.DELTA)) match {
            case Failure(exception) =>
              logger
                .error(exception)("Error refreshing") *>
                MonadError[F, Throwable].raiseError(exception)
            case Success(_) => Monad[F].unit
          }
        }
        .as(())

}
