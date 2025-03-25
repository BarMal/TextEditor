package app.terminal

import cats.effect.kernel.Async
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}

import scala.language.postfixOps

object Term {

  def createF[F[_]: Async]: F[Terminal] = Async[F].blocking {
    val term = DefaultTerminalFactory()
      .setTerminalEmulatorTitle("Tedit")
      .setInitialTerminalSize(new TerminalSize(75, 50))
      .createTerminal()

    term.enterPrivateMode()
    term.setCursorVisible(false)
    term
  }

  def closeTerminalF[F[_]: Async]: Terminal => F[Unit] = terminal =>
    Async[F].blocking {
      terminal.exitPrivateMode()
      terminal.close()
    }

}
