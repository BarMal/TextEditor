package app.terminal

import cats.effect.kernel.Async
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.terminal.swing.{AWTTerminalFontConfiguration, SwingTerminalFontConfiguration}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}

import java.awt.Font
import scala.language.postfixOps

object Term {

  private val fontConfiguration: AWTTerminalFontConfiguration =
    AWTTerminalFontConfiguration.newInstance(Font("Noto Mono", 0, 48))

  private val fontConfiguration1: SwingTerminalFontConfiguration  =
    SwingTerminalFontConfiguration.newInstance(Font("DejaVu Sans Mono", 0, 16))

  def createF[F[_]: Async]: F[Terminal] = Async[F].blocking {
    val term = DefaultTerminalFactory()
      .setTerminalEmulatorTitle("Tedit")
      .setInitialTerminalSize(new TerminalSize(75, 50))
      .setTerminalEmulatorFontConfiguration(fontConfiguration1)
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
