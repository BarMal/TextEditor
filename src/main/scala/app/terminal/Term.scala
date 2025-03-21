package app.terminal

import cats.effect.kernel.Async
import cats.implicits.toFunctorOps
import cats.{Applicative, Show}
import com.googlecode.lanterna.{input, TerminalSize}
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import scala.concurrent.duration.DurationInt

import scala.language.postfixOps

class Term[F[_]: Async](terminal: Terminal) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private val fTerm = Async[F].blocking(terminal)

  private val cursorTimerStream: fs2.Stream[F, Boolean] =
    fs2.Stream.awakeEvery(500 milliseconds).evalScan(false) {
      case (visible, _) => Applicative[F].pure(!visible)
    }

  def _print[T: Show](t: T, cursorPos: Int): fs2.Stream[F, Boolean] =
    cursorTimerStream.evalTap { visible =>
      fTerm.map { term =>
        term.clearScreen()
        term.setCursorVisible(visible)
        Show[T].show(t).foreach { char =>
          try term.putCharacter(char)
          catch
            case ex =>
              logger.error(ex)(s"""Error printing character for $t""")
          term.putCharacter
        }
        term.flush()
      }
    }
  //    fTerm.map { term =>
//      term.clearScreen()
//      Show[T].show(t).foreach { char =>
//        try term.putCharacter(char)
//        catch
//          case ex =>
//            logger.error(ex)(s"""Error printing character for $t""")
//        term.putCharacter
//      }
//      term.flush()
//    }

  def print[T: Show](t: T): F[Unit] =
    fTerm.map { term =>
      term.clearScreen()
      Show[T].show(t).foreach { char =>
        try term.putCharacter(char)
        catch
          case ex =>
            logger.error(ex)(s"""Error printing character for $t""")
        term.putCharacter
      }
      term.flush()
    }

  def readStream: fs2.Stream[F, KeyStroke] =
    fs2.Stream.repeatEval(fTerm.map(_.readInput()))

  def close: F[Unit] = Async[F].blocking(terminal.close())

}

object Term {
  def createF[F[_]: Async]: F[Term[F]] = Applicative[F].pure {
    val term = DefaultTerminalFactory()
      .setTerminalEmulatorTitle("")
      .setInitialTerminalSize(new TerminalSize(75, 50))
      .createTerminal()
    term.enterPrivateMode()
    new Term(term)
  }
}
