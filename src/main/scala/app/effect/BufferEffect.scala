package app.effect

import app.State
import IntOps.*

trait BufferEffect extends Effect {
  def effect: State => State
}

object BufferEffect {
  case class Write(char: Char) extends BufferEffect {

    def wrapLine: State => State = state =>
      if (
        state.buffer.isEmpty || (state.cursorPosition % state.lineLength != 0)
      )
        State(
          state.buffer.insert(state.cursorPosition, char),
          state.cursorPosition + 1,
          this :: state.userEffects,
          state.lineLength
        )
      else {
        val lastWhitespace = state.buffer.lastIndexOf(" ")
        val nextNewLineIndex = state.buffer
          .lastIndexOf('\n')
          .whenNotFound(lastWhitespace)
          .whenNotFound(state.buffer.length)
        State(
          state.buffer.insert(nextNewLineIndex, " ").append(char),
          state.cursorPosition + 2,
          this :: state.userEffects,
          state.lineLength
        )
      }

    override def effect: State => State = state =>
      State(
        state.buffer.insert(state.cursorPosition, char),
        state.cursorPosition + 1,
        this :: state.userEffects,
        state.lineLength
      )
  }

  case object Return extends BufferEffect {
    override def effect: State => State = state =>
      State(
        state.buffer.insert(state.cursorPosition, '\n'),
        state.cursorPosition + 1,
        this :: state.userEffects,
        state.lineLength
      )
  }
}

object IntOps {
  extension (a: Int) {
    def whenNotFound: Int => Int = b => if (a == -1) b else a
  }
}