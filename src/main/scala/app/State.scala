package app

import app.Renderable.{Body, Header}
import app.UserInput.flatten
import app.effect.{BufferEffect, CursorOnlyEffect, Effect}
import cats.Show
import com.googlecode.lanterna.input.KeyStroke

class State(
    val buffer: StringBuilder,
    val cursorPosition: Int,
    val userEffects: List[Effect],
    val lineLength: Int
) {

  def exitCondition: Boolean = true
//    !userEffects.lastOption.contains(Effect.Escape)

  def ++(in: KeyStroke): State = UserInput.keyStrokeToEffect(in.flatten) match
    case effect: BufferEffect     => effect.effect(this)
    case effect: CursorOnlyEffect => effect.effect(this)
    case others =>
      State(buffer, cursorPosition, others :: userEffects, lineLength)
}

object State {

  def empty = new State(new StringBuilder(""), 0, List.empty[Effect], 30)

  given showInstance: Show[State] = (t: State) =>
    Show[Header].show(Header(t)) ++ "\n\n" ++ Show[Body].show(Body(t))

}
