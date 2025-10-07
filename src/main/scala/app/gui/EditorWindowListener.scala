package app.gui

import app.algebra.{LayoutAlgebra, WindowEvent, WindowEventAlgebra}
import cats.effect.unsafe.IORuntime
import cats.effect.IO
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.gui2.{Window, WindowListener}
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Window listener that publishes events to a stream for handling in F context
 * F must be IO since we need to use unsafeRunSync for WindowListener interface compatibility
 */
class EditorWindowListener(
    eventAlg: WindowEventAlgebra[IO]
)(using runtime: IORuntime) extends WindowListener {
  
  override def onResized(
      window: Window,
      oldSize: TerminalSize,
      newSize: TerminalSize
  ): Unit = {
    // Just publish the event, let the handler deal with validation
    eventAlg.publish(WindowEvent.Resized(oldSize, newSize)).unsafeRunSync()
  }

  override def onMoved(
      window: Window,
      oldPosition: com.googlecode.lanterna.TerminalPosition,
      newPosition: com.googlecode.lanterna.TerminalPosition
  ): Unit = {
    eventAlg.publish(WindowEvent.Moved(oldPosition, newPosition)).unsafeRunSync()
  }

  override def onInput(
      window: Window,
      keyStroke: com.googlecode.lanterna.input.KeyStroke,
      deliver: AtomicBoolean
  ): Unit = {
    eventAlg.publish(WindowEvent.Input(keyStroke)).unsafeRunSync()
  }

  override def onUnhandledInput(
      window: Window,
      keyStroke: com.googlecode.lanterna.input.KeyStroke,
      deliver: AtomicBoolean
  ): Unit = {
    eventAlg.publish(WindowEvent.UnhandledInput(keyStroke)).unsafeRunSync()
  }
}
