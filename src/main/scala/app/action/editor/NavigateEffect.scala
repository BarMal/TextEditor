package app.action.editor

import app.State

sealed trait NavigateEffect extends BufferEffect {

  def boundsCheck: State => Boolean

  def moveCursor: State => Int

  def effect: State => State =
    state =>
      State(
        buffer = state.buffer,
        cursorPosition =
          if (boundsCheck(state)) moveCursor(state) else state.cursorPosition,
        userEffects = this :: state.userEffects,
        lineLength = state.lineLength,
        selected = None
      )
}

object NavigateEffect {

  case object CursorLeft extends NavigateEffect {
    override def moveCursor: State => Int = state => state.cursorPosition - 1
    override def boundsCheck: State => Boolean = state => moveCursor(state) >= 0
  }

  case object CursorRight extends NavigateEffect {
    override def moveCursor: State => Int = state => state.cursorPosition + 1
    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) <= state.buffer.length()
  }

  case object CursorUp extends NavigateEffect {
    override def moveCursor: State => Int = state =>
      state.cursorPosition - state.lineLength
    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) > state.buffer.length()
  }

  case object CursorDown extends NavigateEffect {
    override def moveCursor: State => Int = state =>
      state.cursorPosition + state.lineLength
    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) < state.buffer.length()
  }

  case object CursorToEnd extends NavigateEffect {
    override def moveCursor: State => Int =
      state =>
        Math.min(
          state.buffer.length,
          state.lineLength * ((state.cursorPosition % state.lineLength) + 1)
        )
    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) < state.buffer.length()
  }

  case object CursorToStart extends NavigateEffect {
    override def moveCursor: State => Int =
      state =>
        Math.max(
          0,
          state.cursorPosition % state.lineLength
        )

    override def boundsCheck: State => Boolean = state => moveCursor(state) >= 0
  }

  case object CursorToTop extends NavigateEffect {
    override def moveCursor: State => Int      = state => state.cursorPosition
    override def boundsCheck: State => Boolean = state => true
  }

  case object CursorToBottom extends NavigateEffect {
    override def moveCursor: State => Int      = state => state.cursorPosition
    override def boundsCheck: State => Boolean = state => true
  }

}
