package app.terminal

import app.render.Element
import cats.effect.kernel.Async
import cats.implicits.{catsSyntaxApplyOps, toFlatMapOps, toTraverseOps}
import cats.{Applicative, Monad, Show}
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.terminal.Terminal
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.util.{Failure, Success, Try}

class Writer[F[_]: Async](terminal: Terminal) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private def defaultTerminalColours: F[Unit] =
    Async[F].blocking {
      terminal.setForegroundColor(ANSI.WHITE)
      terminal.setBackgroundColor(ANSI.BLACK)
    }

  private def clear: F[Unit] = Async[F].blocking(terminal.clearScreen())

  private def flush: F[Unit] = Async[F].blocking(terminal.flush())

  private def safePrint: Char => F[Unit] = char =>
    Async[F]
      .blocking(Try(terminal.putCharacter(char)))
      .flatMap {
        case Success(_) => Applicative[F].unit
        case Failure(exception) =>
          logger.error(exception)(s"""Error printing character""")
      }

  private def setColours(
      foreground: TextColor.ANSI,
      background: TextColor.ANSI
  ): F[Unit] =
    Async[F].blocking {
      terminal.setForegroundColor(foreground)
      terminal.setBackgroundColor(background)
    }

  private def _print[T: Show](showable: T): F[Unit] =
    Monad[F].void(Show[T].show(showable).toList.traverse(safePrint))

  private def _printWithColours[T: Show](
      showable: T,
      foreground: TextColor.ANSI,
      background: TextColor.ANSI
  ): F[Unit] =
    setColours(foreground, background) *>
      _print(showable) *>
      defaultTerminalColours

  def print(elements: List[Element]): F[Unit] =
    clear *> elements.traverse(elem =>
      _printWithColours[String](
        elem.repr,
        elem.foregroundColour,
        elem.backgroundColour
      )
    ) *> flush

}
