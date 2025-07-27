package app

import app.action.Effect
import app.action.Effect.{Escape, Unexpected}
import app.action.editor.{
  DeleteEffect,
  NavigateEffect,
  StateChangeEffect,
  WriteEffect
}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}

case class UserInput(
    character: Option[Char],
    keyType: KeyType,
    modifiers: Vector[Modifier],
    time: Long
)

object UserInput {

  def keyStrokeToEffect(input: UserInput): Effect = input.keyType match
    case KeyType.Enter      => WriteEffect.Return
    case KeyType.Tab        => WriteEffect.Tab
    case KeyType.Backspace  => DeleteEffect.DeleteLeft(input.modifiers)
    case KeyType.ArrowLeft  => NavigateEffect.CursorLeft(input.modifiers)
    case KeyType.ArrowRight => NavigateEffect.CursorRight(input.modifiers)
    case KeyType.ArrowUp    => NavigateEffect.CursorUp(input.modifiers)
    case KeyType.ArrowDown  => NavigateEffect.CursorDown(input.modifiers)
    case KeyType.Delete     => DeleteEffect.DeleteRight(input.modifiers)
    case KeyType.Home       => NavigateEffect.CursorToStart(input.modifiers)
    case KeyType.End        => NavigateEffect.CursorToEnd(input.modifiers)
    case KeyType.PageUp     => NavigateEffect.CursorToTop(input.modifiers)
    case KeyType.PageDown   => NavigateEffect.CursorToBottom(input.modifiers)
    case KeyType.Escape     => Escape
    case KeyType.Insert     => StateChangeEffect.ToggleWriteMode
    case KeyType.Unknown    => Unexpected(input)
    case KeyType.Character =>
      WriteEffect.TogglingWrite(input.character.getOrElse('?'))
    case _                  => Unexpected(input)
    case KeyType.ReverseTab => ???
    case KeyType.F1         => ???
    case KeyType.F2         => ???
    case KeyType.F3         => ???
    case KeyType.F4         => ???
    case KeyType.F5         => ???
    case KeyType.F6         => ???
    case KeyType.F7         => ???
    case KeyType.F8         => ???
    case KeyType.F9         => ???
    case KeyType.F10        => ???
    case KeyType.F11        => ???
    case KeyType.F12        => ???
    case KeyType.F13        => ???
    case KeyType.F14        => ???
    case KeyType.F15        => ???
    case KeyType.F16        => ???
    case KeyType.F17        => ???
    case KeyType.F18        => ???
    case KeyType.F19        => ???
    case KeyType.CursorLocation => ???
    case KeyType.MouseEvent     => ???
    case KeyType.EOF            => ???

  extension (ks: KeyStroke) {
    def flatten: UserInput =
      UserInput(
        Option(ks.getCharacter).map(_.toChar),
        ks.getKeyType,
        Modifier(ks),
        ks.getEventTime
      )
  }

}

enum Modifier:
  case Control
  case Alt
  case Shift

object Modifier {
  def apply(keyStroke: KeyStroke): Vector[Modifier] =
    Vector(
      Option.when(keyStroke.isCtrlDown)(Modifier.Control),
      Option.when(keyStroke.isAltDown)(Modifier.Alt),
      Option.when(keyStroke.isShiftDown)(Modifier.Shift)
    ).flatten
}
