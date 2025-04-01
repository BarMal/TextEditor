package app.action.editor

import app.{BufferState, Modifier, Selected}

sealed trait NavigateEffect(modifiers: List[Modifier]) extends BufferEffect {

  def boundsCheck: BufferState => Boolean

  def moveCursor: BufferState => Int

  private def selectionChange: BufferState => Option[Selected] =
    state =>
      state.selected match
        case Some(Selected(start, end)) =>
          Some(
            Selected(
              start,
              if boundsCheck(state) then moveCursor(state) else end
            )
          )
        case None =>
          Some(
            Selected(
              state.cursorPosition,
              if boundsCheck(state) then moveCursor(state)
              else state.cursorPosition
            )
          )

  def effect: BufferState => BufferState =
    state =>
      BufferState(
        buffer = state.buffer,
        cursorPosition =
          if (boundsCheck(state)) moveCursor(state) else state.cursorPosition,
        userEffects = this :: state.userEffects,
        lineLength = state.lineLength,
        selected =
          if modifiers.contains(Modifier.Shift) then selectionChange(state)
          else None,
        writeMode = state.writeMode
      )
}

object NavigateEffect {

  case class CursorLeft(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state =>
      state.cursorPosition - 1
    override def boundsCheck: BufferState => Boolean = state =>
      moveCursor(state) >= 0
  }

  case class CursorRight(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state =>
      state.cursorPosition + 1
    override def boundsCheck: BufferState => Boolean = state =>
      moveCursor(state) <= state.buffer.weight
  }

  case class CursorUp(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state =>
      state.cursorPosition - state.lineLength
    override def boundsCheck: BufferState => Boolean = state =>
      0 <= moveCursor(state)
  }

  case class CursorDown(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state =>
      state.cursorPosition + state.lineLength
    override def boundsCheck: BufferState => Boolean = state =>
      moveCursor(state) <= state.buffer.weight
  }

  case class CursorToEnd(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state =>
      Math.min(state.buffer.weight, (1 + (state.cursorPosition / state.lineLength)) * state.lineLength)
    override def boundsCheck: BufferState => Boolean = state => true
  }

  case class CursorToStart(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int =
        state =>
          Math.max(state.cursorPosition - (state.cursorPosition % state.lineLength), 0)
    override def boundsCheck: BufferState => Boolean = state => true
  }

  case class CursorToTop(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state => state.cursorPosition
    override def boundsCheck: BufferState => Boolean = state => true
  }

  case class CursorToBottom(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor: BufferState => Int = state => state.cursorPosition
    override def boundsCheck: BufferState => Boolean = state => true
  }

}
