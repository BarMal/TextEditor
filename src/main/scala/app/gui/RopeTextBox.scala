package app.gui

import app.buffer.rope.Rope
import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.{TerminalPosition, TerminalSize, TextColor}
import com.googlecode.lanterna.gui2.Interactable.Result

class RopeTextBox(var rope: Rope)
    extends AbstractInteractableComponent[RopeTextBox] {

  private var cursorX: Int = 0
  private var cursorY: Int = 0

  override def handleKeyStroke(key: KeyStroke): Result = {
    key.getKeyType match {
      case KeyType.Character =>
        rope = rope.insert(cursorPosition, key.getCharacter.toString)
        cursorX += 1
      case KeyType.Backspace =>
        rope = rope.delete(cursorPosition - 1, cursorPosition)
        cursorX = math.max(0, cursorX - 1)
      case KeyType.ArrowLeft =>
        cursorX = math.max(0, cursorX - 1)
      case KeyType.ArrowRight =>
        cursorX += 1
      case _ => // ignore others for now
    }
    Result.HANDLED
  }

//  override def createDefaultRenderer(): ComponentRenderer[RopeTextBox] =
//    new ComponentRenderer[RopeTextBox] {
//
//      override def getPreferredSize(component: RopeTextBox): TerminalSize =
//        component.getPreferredSize
//
//      override def drawComponent(
//          graphics: TextGUIGraphics,
//          component: RopeTextBox
//      ): Unit = {
//        graphics.setForegroundColor(TextColor.ANSI.WHITE)
//        graphics.setBackgroundColor(TextColor.ANSI.BLACK)
//        graphics.putString(0, 0, component.rope.toString.take(2000))
//        graphics.setCharacter(component.cursorX, component.cursorY, '_')
//      }
//    }

  private def cursorPosition: Int = cursorY * 80 + cursorX

  override def createDefaultRenderer(): InteractableRenderer[RopeTextBox] =
    new InteractableRenderer[RopeTextBox] {
      override def getCursorLocation(component: RopeTextBox): TerminalPosition =
        TerminalPosition(cursorX, cursorY)

      override def getPreferredSize(component: RopeTextBox): TerminalSize =
        component.getPreferredSize

      override def drawComponent(
          graphics: TextGUIGraphics,
          component: RopeTextBox
      ): Unit = {
        graphics.setForegroundColor(TextColor.ANSI.WHITE)
        graphics.setBackgroundColor(TextColor.ANSI.BLACK)
        graphics.putString(0, 0, component.rope.collect().take(2000))
        graphics.setCharacter(component.cursorX, component.cursorY, '_')
      }
    }
}
