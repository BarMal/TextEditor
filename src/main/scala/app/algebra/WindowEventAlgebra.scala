package app.algebra

import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.input.KeyStroke
import fs2.Stream

/**
 * Algebra for window events
 */
sealed trait WindowEvent
object WindowEvent {
  case class Resized(oldSize: TerminalSize, newSize: TerminalSize) extends WindowEvent
  case class Moved(oldPos: TerminalPosition, newPos: TerminalPosition) extends WindowEvent
  case class Input(keyStroke: KeyStroke) extends WindowEvent
  case class UnhandledInput(keyStroke: KeyStroke) extends WindowEvent
}

trait WindowEventAlgebra[F[_]] {
  def events: Stream[F, WindowEvent]
  def publish(event: WindowEvent): F[Unit]
}
