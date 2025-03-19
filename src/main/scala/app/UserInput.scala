package app

import app.Modifier.Shift
import app.effect.Effect.Unexpected
import app.effect.{BufferEffect, CursorOnlyEffect, Effect, RemoveEffect}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}

case class UserInput(
    character: Option[Character],
    keyType: KeyType,
    modifiers: List[Modifier],
    time: Long
)

object UserInput {

  def combinationHandler(input: UserInput): Effect = Unexpected(input)

  def nonPrintingHandler(input: UserInput): Effect = input.keyType match
    case KeyType.Character => Unexpected(input)
    case KeyType.Backspace => RemoveEffect.Backspace
    case KeyType.ArrowLeft => CursorOnlyEffect.LeftArrow
    case KeyType.ArrowRight => CursorOnlyEffect.RightArrow
    case KeyType.ArrowUp => CursorOnlyEffect.UpArrow
    case KeyType.ArrowDown => CursorOnlyEffect.DownArrow
    case KeyType.Delete => RemoveEffect.Delete
    case KeyType.Home => CursorOnlyEffect.Home
    case KeyType.End => CursorOnlyEffect.End
    case KeyType.PageUp => CursorOnlyEffect.PageUp
    case KeyType.PageDown => CursorOnlyEffect.PageDown
    case _ => Unexpected(input)
    case KeyType.Insert => ???
    case KeyType.Escape => ???
    case KeyType.Tab => ???
    case KeyType.ReverseTab => ???
    case KeyType.Enter => ???
    case KeyType.F1 => ???
    case KeyType.F2 => ???
    case KeyType.F3 => ???
    case KeyType.F4 => ???
    case KeyType.F5 => ???
    case KeyType.F6 => ???
    case KeyType.F7 => ???
    case KeyType.F8 => ???
    case KeyType.F9 => ???
    case KeyType.F10 => ???
    case KeyType.F11 => ???
    case KeyType.F12 => ???
    case KeyType.F13 => ???
    case KeyType.F14 => ???
    case KeyType.F15 => ???
    case KeyType.F16 => ???
    case KeyType.F17 => ???
    case KeyType.F18 => ???
    case KeyType.F19 => ???
    case KeyType.Unknown => ???
    case KeyType.CursorLocation => ???
    case KeyType.MouseEvent => ???
    case KeyType.EOF => ???

  def keyStrokeToEffect(input: UserInput): Effect =
    input.modifiers match
      case Shift :: Nil | Nil if input.keyType == KeyType.Character =>
        BufferEffect.Write(input.character.map(_.toChar).getOrElse(' '))
      case Nil => nonPrintingHandler(input)
      case _   => combinationHandler(input)

//  def _keyStrokeToEffect(keyStroke: KeyStroke): Effect = {
//    if (keyStroke.isAltDown || keyStroke.isCtrlDown) modifierHandler(keyStroke)
//    else BufferEffect.Write(keyStroke.getCharacter)
//    keyStroke.getKeyType match
//      case KeyType.Character => BufferEffect.Write(keyStroke.getCharacter)
////      case KeyType.Escape => ???
//      case KeyType.Backspace  => RemoveEffect.Backspace
//      case KeyType.ArrowLeft  => CursorOnlyEffect.LeftArrow
//      case KeyType.ArrowRight => CursorOnlyEffect.RightArrow
//      case KeyType.ArrowUp    => CursorOnlyEffect.UpArrow
//      case KeyType.ArrowDown  => CursorOnlyEffect.DownArrow
////      case KeyType.Insert => ???
//      case KeyType.Delete => RemoveEffect.Delete
//      case KeyType.Home   => CursorOnlyEffect.Home
//      case KeyType.End    => CursorOnlyEffect.End
////      case KeyType.PageUp => ???
////      case KeyType.PageDown => ???
////      case KeyType.Tab => ???
////      case KeyType.ReverseTab => ???
//      case KeyType.Enter => BufferEffect.Return
////      case KeyType.F1 => ???
////      case KeyType.F2 => ???
////      case KeyType.F3 => ???
////      case KeyType.F4 => ???
////      case KeyType.F5 => ???
////      case KeyType.F6 => ???
////      case KeyType.F7 => ???
////      case KeyType.F8 => ???
////      case KeyType.F9 => ???
////      case KeyType.F10 => ???
////      case KeyType.F11 => ???
////      case KeyType.F12 => ???
////      case KeyType.F13 => ???
////      case KeyType.F14 => ???
////      case KeyType.F15 => ???
////      case KeyType.F16 => ???
////      case KeyType.F17 => ???
////      case KeyType.F18 => ???
////      case KeyType.F19 => ???
////      case KeyType.Unknown => ???
////      case KeyType.CursorLocation => ???
////      case KeyType.MouseEvent => ???
////      case KeyType.EOF => ???
//      case _ => Experimental(keyStroke)
//  }

  extension (ks: KeyStroke) {
    def flatten: UserInput =
      UserInput(Option(ks.getCharacter), ks.getKeyType, Modifier(ks), ks.getEventTime)
  }

}

sealed trait Modifier

object Modifier {
  case object Control extends Modifier
  case object Alt     extends Modifier
  case object Shift   extends Modifier

  def apply(keyStroke: KeyStroke): List[Modifier] =
    List(
      Option.when(keyStroke.isCtrlDown)(Modifier.Control),
      Option.when(keyStroke.isAltDown)(Modifier.Alt),
      Option.when(keyStroke.isShiftDown)(Modifier.Shift)
    ).flatten
}
