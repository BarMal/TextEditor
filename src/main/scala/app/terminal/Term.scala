package app.terminal

import app.Renderable.{Body, Header}
import app.{BufferState, Renderable}
import cats.effect.kernel.Async
import cats.implicits.{catsSyntaxApplyOps, toFunctorOps}
import cats.{Applicative, Show}
import com.googlecode.lanterna.TextColor.*
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.googlecode.lanterna.{input, TerminalSize, TextColor}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import cats.implicits.toFlatMapOps
import cats.implicits.toTraverseOps

import scala.language.postfixOps

class Term[F[_]: Async](
    terminal: Terminal
) {

  private val logger: SelfAwareStructuredLogger[F] =
    Slf4jFactory.create[F].getLogger

  private lazy val fTerm: F[Terminal] = Async[F].blocking(terminal)

  private def defaultTerminalColours(): F[Unit] = fTerm.map { term =>
    term.setBackgroundColor(ANSI.BLACK)
    term.setForegroundColor(ANSI.WHITE)
  }

  private def safePrint: Char => F[Unit] = char =>
    fTerm.map { term =>
      try term.putCharacter(char)
      catch
        case ex =>
          logger.error(ex)(s"""Error printing character""")
    }

  private def printHeader(header: Header): F[Unit] = fTerm.flatMap { term =>
    term.setBackgroundColor(ANSI.YELLOW)
    term.setForegroundColor(ANSI.WHITE)
    Show[Header]
      .show(header)
      .appended('\n')
      .toList
      .traverse(safePrint)
  }.void

  private def printBody(
      body: Body,
      cursorPosition: Int,
      cursorVisible: Boolean
  ): F[Unit] =
    fTerm.flatMap { _ =>
      val (pre, post)     = (Show[Body].show(body) + " ").splitAt(cursorPosition + 1)
      val (charPos, rest) = post.splitAt(1)
      val cursor          = if cursorVisible then '\u2588' else charPos
      val buffer          = (pre + cursor) + rest
      buffer.toList.traverse(safePrint)
    } *> defaultTerminalColours()

  def print(state: BufferState, cursorVisible: Boolean): F[Unit] =
    print(Header(state), Body(state), state.cursorPosition, cursorVisible)

  private def print(
      header: Header,
      body: Body,
      cursorPosition: Int,
      cursorVisible: Boolean
  ): F[Unit] =
    for {
      _ <- cls()
      _ <- printHeader(header)
      _ <- defaultTerminalColours()
      _ <- printBody(body, cursorPosition, cursorVisible)
      _ <- defaultTerminalColours()
      _ <- flush()
    } yield ()

  def cls(): F[Unit]   = fTerm.map(_.clearScreen())
  def flush(): F[Unit] = fTerm.map(_.flush())

//  def print[R <: Renderable: Show](renderables: List[R]): F[Unit] = fTerm.map {
//    term =>
//      term.clearScreen()
//      renderables.foreach { renderable => ??? }
//      term.flush()
//  }

//  def print[T: Show](
//      t: T,
//      cursorPosition: (Int, Int),
//      cursorVisibility: Boolean
//  ): F[Unit] =
//    fTerm.map { term =>
//      term.clearScreen()
//      Show[T].show(t).foreach { char =>
//        try term.putCharacter(char)
//        catch
//          case ex =>
//            logger.error(ex)(s"""Error printing character for $t""")
//      }
//      term.flush()
//    }

  def readStream: fs2.Stream[F, KeyStroke] =
    fs2.Stream.repeatEval(fTerm.map(_.readInput()))

  def close: F[Unit] = fTerm.map { term =>
    term.exitPrivateMode()
    term.close()
  }

}

object Term {
  def createF[F[_]: Async]: F[Term[F]] =
    Applicative[F].pure {
      val term = DefaultTerminalFactory()
        .setTerminalEmulatorTitle("")
        .setInitialTerminalSize(new TerminalSize(75, 50))
        .createTerminal()
      term.enterPrivateMode()
      term.setCursorVisible(false)
      new Term(term)
    }
}
