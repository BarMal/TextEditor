package app

import app.effect.Effect.Undefined
import cats.Show
import effect.{BufferEffect, CursorOnlyEffect, Effect}

class State(
    val buffer: StringBuilder,
    val cursorPosition: Int,
    val userEffects: List[Effect],
    val lineLength: Int
) {

  def ++(in: Effect): State = in match
    case effect: BufferEffect     => effect.effect(this)
    case effect: CursorOnlyEffect => effect.effect(this)
    case others =>
      State(buffer, cursorPosition, others :: userEffects, lineLength)
}

object State {

  def empty = new State(new StringBuilder(""), 0, List.empty[Effect], 30)

  given showInstance: Show[State] = (t: State) =>
    s"""Cursor position: ${t.cursorPosition} | Buffer size: ${t.buffer.length} | Last effect: ${t.userEffects.headOption
        .getOrElse(Undefined(Nil))}\n${t.buffer.mkString}"""

}
