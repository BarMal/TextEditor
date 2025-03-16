package app.terminal

import cats.effect.kernel.Async
import cats.implicits.toFunctorOps
import cats.{Applicative, Show}
import com.googlecode.lanterna.input
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import scala.concurrent.duration.DurationInt

import scala.language.postfixOps

class Term[F[_]: Async](terminal: Terminal) {

  private val fTerm = Async[F].blocking(terminal)

  def print[T: Show](ts: T*): F[Unit] =
    fTerm.map { term =>
      term.clearScreen()
      ts.toList.foreach(t => Show[T].show(t).foreach(term.putCharacter))
      term.flush()
    }

  def read: F[input.KeyStroke] = fTerm.map(_.readInput())

  def readStream: fs2.Stream[F, KeyStroke] =
    fs2.Stream.repeatEval(fTerm.map(_.readInput()))

  def close: F[Unit] = Async[F].blocking(terminal.close())

}

object Term {
  def createF[F[_]: Async]: F[Term[F]] = Applicative[F].pure {
    val term = DefaultTerminalFactory().createTerminal()
    term.enterPrivateMode()
    new Term(term)
  }
}
