package app.terminal

import app.BufferState
import cats.effect.kernel.Async
import cats.implicits.toFunctorOps
import cats.{Applicative, Show}
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.swing.AWTTerminalFontConfiguration
import com.googlecode.lanterna.terminal.swing.AWTTerminalFontConfiguration.BoldMode
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.googlecode.lanterna.{TerminalSize, input}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.awt.Font
import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps

class Term[F[_]: Async](
    terminal: Terminal
) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private lazy val fTerm: F[Terminal] = Async[F].blocking(terminal)

//  def renderStream(in: fs2.Stream[F, BufferState]): fs2.Stream[F, Unit] =
//    in.evalTap { state =>
//      print(
//        state,
//        (
//          Math.floorMod(state.cursorPosition, state.lineLength),
//          Math.floorDiv(state.cursorPosition, state.lineLength)
//        ),
//        Math.floorDiv(System.currentTimeMillis() - startTime, cursorBlinkInterval.toMillis) % 2 == 0
//      )
//    }.void

  def print[T: Show](
      t: T,
      cursorPosition: (Int, Int),
      cursorVisibility: Boolean
  ): F[Unit] =
    fTerm.map { term =>
      term.clearScreen()
      term.setCursorVisible(cursorVisibility)
      term.setCursorPosition(cursorPosition._1, cursorPosition._2)
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

  def close: F[Unit] = fTerm.map(_.close())

}

object Term {
  def createF[F[_]: Async]: F[Term[F]] =
    Applicative[F].pure {
      val term = DefaultTerminalFactory()
        .setTerminalEmulatorTitle("")
        .setInitialTerminalSize(new TerminalSize(75, 50))
        .createTerminal()
      term.enterPrivateMode()
      new Term(term)
    }
}
