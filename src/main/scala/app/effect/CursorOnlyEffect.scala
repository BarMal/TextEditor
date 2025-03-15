package app.effect

import app.State

sealed trait CursorOnlyEffect extends Effect {

  def boundsCheck: State => Boolean

  def moveCursor: State => Int

  def effect: State => State =
    state =>
      State(
        state.buffer,
        if (boundsCheck(state)) moveCursor(state)
        else state.cursorPosition,
        this :: state.userEffects,
        state.lineLength
      )
}

object CursorOnlyEffect {

  case object LeftArrow extends CursorOnlyEffect {
    override def moveCursor: State => Int = state => state.cursorPosition - 1

    override def boundsCheck: State => Boolean = state => moveCursor(state) >= 0
  }

  case object RightArrow extends CursorOnlyEffect {
    override def moveCursor: State => Int = state => state.cursorPosition + 1

    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) <= state.buffer.length()
  }

  case object UpArrow extends CursorOnlyEffect {
    override def moveCursor: State => Int = state =>
      state.cursorPosition - state.lineLength

    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) > state.buffer.length()
  }

  case object DownArrow extends CursorOnlyEffect {
    override def moveCursor: State => Int = state =>
      state.cursorPosition + state.lineLength

    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) < state.buffer.length()
  }

  case object End extends CursorOnlyEffect {
    override def moveCursor: State => Int =
      state =>
        Math.min(
          state.buffer.length,
          state.lineLength * ((state.cursorPosition % state.lineLength) + 1)
        )

    override def boundsCheck: State => Boolean = state =>
      moveCursor(state) < state.buffer.length()
  }

  case object Home extends CursorOnlyEffect {
    override def moveCursor: State => Int =
      state =>
        Math.max(
          0,
          state.cursorPosition % state.lineLength
        )

    override def boundsCheck: State => Boolean = state => moveCursor(state) >= 0
  }

  case object PageUp extends CursorOnlyEffect {
    override def moveCursor: State => Int = state => state.cursorPosition

    override def boundsCheck: State => Boolean = state => true
  }

  case object PageDown extends CursorOnlyEffect {
    override def moveCursor: State => Int = state => state.cursorPosition

    override def boundsCheck: State => Boolean = state => true
  }

}
