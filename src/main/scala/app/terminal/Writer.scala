package app.terminal

import app.render.Element
import cats.{Applicative, Show}
import cats.effect.kernel.Async
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.terminal.Terminal
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import cats.implicits.{catsSyntaxApplyOps, toTraverseOps}
import com.googlecode.lanterna.TextColor

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
    try Async[F].blocking(terminal.putCharacter(char))
    catch
      case ex =>
        logger.error(ex)(s"""Error printing character""")

  private def setColours: TextColor.ANSI => TextColor.ANSI => F[Unit] =
    foreground =>
      background =>
        Async[F].blocking {
          terminal.setForegroundColor(foreground)
          terminal.setBackgroundColor(background)
        }

  private def _print[T: Show]: T => F[Unit] = showable =>
    Show[T].show(showable).toList.traverse(safePrint).void

  private def _printWithColours[T: Show]
      : T => TextColor.ANSI => TextColor.ANSI => F[Unit] =
    showable =>
      foreground =>
        background =>
          setColours(foreground)(background) *>
            _print(showable) *>
            defaultTerminalColours

  def print(elements: Element*): F[Unit] =
    clear *> elements.traverse(elem =>
      _printWithColours[String](elem.repr)(elem.foregroundColour)(
        elem.backgroundColour
      )
    ) *> flush

  extension [A](f: F[A]) {
    def void: F[Unit] = Applicative[F].void(f)
  }

}
