package app

import cats.Show
import com.googlecode.lanterna.input.KeyStroke
import effect.{BufferEffect, CursorOnlyEffect, Effect}
import app.UserInput.flatten
import app.effect.Effect.Unexpected

class State(
    val buffer: StringBuilder,
    val cursorPosition: Int,
    val userEffects: List[Effect],
    val lineLength: Int
) {

  def exitCondition: Boolean = !userEffects.lastOption.contains(Effect.Escape)

  def ++(in: KeyStroke): State = UserInput.keyStrokeToEffect(in.flatten) match
    case effect: BufferEffect     => effect.effect(this)
    case effect: CursorOnlyEffect => effect.effect(this)
    case others =>
      State(buffer, cursorPosition, others :: userEffects, lineLength)
}

object State {

  def empty = new State(new StringBuilder(""), 0, List.empty[Effect], 30)

  given showInstance: Show[State] = (t: State) =>
    s"""Cursor position: ${t.cursorPosition} | Buffer size: ${t.buffer.length} | Last effect: ${t.userEffects.headOption
        .getOrElse(Unexpected())}\n${t.buffer.mkString}"""

}
