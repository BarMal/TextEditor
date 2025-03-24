package app

import app.Renderable.{Body, Header}
import app.UserInput.flatten
import app.action.Effect
import app.action.editor.{DeleteEffect, NavigateEffect, WriteEffect}
import cats.Show
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

//given stateMonoid: Monoid[BufferState] = new Monoid[BufferState] {
//  override def empty: BufferState = BufferState.empty
//
//  override def combine(x: BufferState, y: BufferState): BufferState =
//    BufferState(
//      buffer = x.buffer.append(y.buffer),
//      cursorPosition = x.buffer.length() + y.buffer.length(),
//      userEffects = x.userEffects ++ y.userEffects,
//      lineLength = Math.max(x.lineLength, y.lineLength),
//      selected = None
//    )
//}
//
//given keyStrokeMonoid: Monoid[KeyStroke] = new Monoid[KeyStroke] {
//
//  override def empty: KeyStroke = KeyStroke
//
//  override def combine(x: KeyStroke, y: KeyStroke): KeyStroke = ???
//}
//
//val foo: State[KeyStroke, BufferState] =
//  State.empty[KeyStroke, BufferState]
//
//val bar = foo.runEmpty

case class BufferState(
    buffer: String,
    cursorPosition: Int,
    userEffects: List[Effect],
    lineLength: Int,
    selected: Option[Range]
) extends Focusable[BufferState] {

  override def ++(in: KeyStroke): BufferState =
    UserInput.keyStrokeToEffect(in.flatten) match
      case effect: WriteEffect    => effect.effect(this)
      case effect: DeleteEffect   => effect.effect(this)
      case effect: NavigateEffect => effect.effect(this)
      case others =>
        BufferState(
          buffer = buffer,
          cursorPosition = cursorPosition,
          userEffects = others :: userEffects,
          lineLength = lineLength,
          selected = None
        )
}

object BufferState {

  def empty =
    new BufferState("", 0, List.empty[Effect], 50, None)

//  given showInstance: Show[BufferState] = (t: BufferState) =>
//    Show[Header].show(Header(t)) ++ "\n\n" ++ Show[Body].show(Body(t))

}
