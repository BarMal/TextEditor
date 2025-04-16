package app

import app.UserInput.flatten
import app.WriteMode.Write
import app.action.Effect
import app.action.editor.{
  DeleteEffect,
  NavigateEffect,
  StateChangeEffect,
  WriteEffect
}
import app.buffer.{Balance, Rope}
import com.googlecode.lanterna.input.KeyStroke

//case class State(
//    bufferState: BufferState,
//    menuState: MenuState,
//    inFocus: Focusable[?]
//) {
//
//  def ++(in: KeyStroke): State = inFocus match
//    case state: MenuState   => this.copy(menuState = state)
//    case state: BufferState => this.copy(bufferState = state)
//
//}

sealed trait Focusable[T <: Focusable[T]] {
  def ++(in: KeyStroke): T
}

case class MenuState() extends Focusable[MenuState] {

  override def ++(in: KeyStroke): MenuState = this
}

sealed trait WriteMode
object WriteMode {
  case object Write     extends WriteMode
  case object Overwrite extends WriteMode

  def flip(mode: WriteMode): WriteMode = mode match
    case Write     => Overwrite
    case Overwrite => Write
}

case class Selected(start: Int, end: Int) {
  def length: Int = Math.abs(start - end)

  override def toString: String = s"""$start - $end"""
}

case class BufferState(
    buffer: Rope,
    cursorPosition: Int,
    userEffects: List[Effect],
    lineLength: Int,
    selected: Option[Selected],
    writeMode: WriteMode
) extends Focusable[BufferState] {

  override def ++(in: KeyStroke): BufferState =
    UserInput.keyStrokeToEffect(in.flatten) match
      case effect: WriteEffect       => effect.effect(this)
      case effect: DeleteEffect      => effect.effect(this)
      case effect: NavigateEffect    => effect.effect(this)
      case effect: StateChangeEffect => effect.effect(this)
      case others =>
        BufferState(
          buffer = buffer,
          cursorPosition = cursorPosition,
          userEffects = others :: userEffects,
          lineLength = lineLength,
          selected = None,
          writeMode = Write
        )
}

object BufferState {

  given balance: Balance = Balance(32, 10, 32)

  def empty =
    new BufferState(Rope(""), 0, List.empty[Effect], 50, None, Write)

}
