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

//implicit val stateMonoid: Monoid[BufferState] = new Monoid[BufferState] {
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
//implicit val keyStrokeMonoid: Monoid[KeyStroke] = new Monoid[KeyStroke] {
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

sealed trait Transform[A, B] {
  def f(a: A): B
  def g(b: B): A
}

case class RefState(
    buffer: String,
    cursorPosition: Int,
    userEffects: List[Effect],
    lineLength: Int,
    selected: Option[Range]
)

object RefState {
  def empty: RefState = RefState("", 0, List.empty[Effect], 30, None)
}

object TransformInstances {

  given bufferToRefState: Transform[BufferState, RefState] =
    new Transform[BufferState, RefState] {

      override def f(a: BufferState): RefState = RefState(
        buffer = a.buffer.result(),
        cursorPosition = a.cursorPosition,
        userEffects = a.userEffects,
        lineLength = a.lineLength,
        selected = a.selected
      )

      override def g(b: RefState): BufferState = BufferState(
        buffer = new StringBuilder(b.buffer),
        cursorPosition = b.cursorPosition,
        userEffects = b.userEffects,
        lineLength = b.lineLength,
        selected = b.selected
      )
    }

}
