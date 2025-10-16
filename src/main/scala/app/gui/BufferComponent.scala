package app.gui

import app.buffer.BufferState
import cats.effect.kernel.Ref
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Sync}
import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.gui2.Interactable.Result
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.{TerminalPosition, TerminalSize, TextColor}

import java.util.concurrent.atomic.AtomicBoolean

final class BufferComponent(
    stateRef: Ref[IO, BufferState],
    blinkStream: fs2.Stream[IO, Unit]
)(using IORuntime)
    extends AbstractInteractableComponent[BufferComponent] {

  // Tracks whether the cursor is visible
  private val cursorVisible = new AtomicBoolean(true)
  // Tracks the character currently used for the cursor
  private val cursorChar =
    new java.util.concurrent.atomic.AtomicReference[Char]('_')

  /** Called by external FS2 tick stream */
  def onBlinkTick(): Unit = {
    // Toggle cursor visibility or character
    if (cursorVisible.get()) cursorChar.set(' ')
    else cursorChar.set('_')
    cursorVisible.set(!cursorVisible.get())
    invalidate() // ask Lanterna to repaint
  }

  /** Handles keystrokes and updates BufferState */
  override def handleKeyStroke(key: KeyStroke): Result = {
    stateRef.update(_.++(key)).unsafeRunAndForget()
    invalidate()
    Result.HANDLED
  }

  /** Renderer that uses BufferState and cursor state */
  override def createDefaultRenderer(): InteractableRenderer[BufferComponent] =
    new InteractableRenderer[BufferComponent] {

      override def getPreferredSize(component: BufferComponent): TerminalSize =
        component.getPreferredSize

      override def drawComponent(
          graphics: TextGUIGraphics,
          component: BufferComponent
      ): Unit = {
        val g = graphics
        g.setBackgroundColor(TextColor.ANSI.BLACK)
        g.setForegroundColor(TextColor.ANSI.WHITE)
        g.fill(' ')

        // Snapshot the current buffer state
        val state = stateRef.get.unsafeRunSync()

        // Draw Rope contents
        val lines = state.buffer.collect().split("\n")
        lines.zipWithIndex.foreach { case (line, y) =>
          if (y < g.getSize.getRows)
            g.putString(0, y, line.take(g.getSize.getColumns))
        }
//        // Draw the cursor
//        val pos = new TerminalPosition(0, 0)
//        if (pos.getRow < g.getSize.getRows) {
//          val c = cursorChar.get()
//          g.setCharacter(
//            pos,
//            new com.googlecode.lanterna.TextCharacter(
//              c,
//              TextColor.ANSI.WHITE,
//              TextColor.ANSI.BLACK
//            )
//          )
//        }
      }

      override def getCursorLocation(
          component: BufferComponent
      ): TerminalPosition = new TerminalPosition(0, 0)
    }
}
