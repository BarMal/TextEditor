package app

import app.Renderable.{Body, Header}
import app.UserInput.flatten
import app.action.Effect
import app.action.editor.{DeleteEffect, NavigateEffect, WriteEffect}
import cats.Show
import com.googlecode.lanterna.input.KeyStroke

case class State(
    bufferState: BufferState,
    menuState: MenuState,
    inFocus: Focusable[?]
) {

  def ++(in: KeyStroke): State = inFocus match
    case state: MenuState   => this.copy(menuState = state)
    case state: BufferState => this.copy(bufferState = state)

}

sealed trait Focusable[T <: Focusable[T]] {
  def ++(in: KeyStroke): T
}

case class MenuState() extends Focusable[MenuState] {

  override def ++(in: KeyStroke): MenuState = this
}

case class BufferState(
    buffer: StringBuilder,
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
    new BufferState(new StringBuilder(""), 0, List.empty[Effect], 30, None)

  given showInstance: Show[BufferState] = (t: BufferState) =>
    Show[Header].show(Header(t)) ++ "\n\n" ++ Show[Body].show(Body(t))

}
