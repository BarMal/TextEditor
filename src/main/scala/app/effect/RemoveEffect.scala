package app.effect

import app.State

trait RemoveEffect extends BufferEffect {
  def boundsCheck: StringBuilder => Int => Boolean
}

object RemoveEffect {
  case object Backspace extends RemoveEffect {
    override def boundsCheck: StringBuilder => Int => Boolean = buffer =>
      cursorPosition => cursorPosition > 0 && cursorPosition <= buffer.length

    override def effect: State => State =
      state =>
        if (boundsCheck(state.buffer)(state.cursorPosition))
          State(
            state.buffer.deleteCharAt(state.cursorPosition - 1),
            state.cursorPosition - 1,
            this :: state.userEffects,
            state.lineLength
          )
        else state
  }

  case object Delete extends RemoveEffect {
    override def boundsCheck: StringBuilder => Int => Boolean = buffer =>
      cursorPosition => cursorPosition >= 0 && cursorPosition < buffer.length

    override def effect: State => State =
      state =>
        if (boundsCheck(state.buffer)(state.cursorPosition))
          State(
            state.buffer.deleteCharAt(state.cursorPosition),
            state.cursorPosition,
            this :: state.userEffects,
            state.lineLength
          )
        else state
  }
}
