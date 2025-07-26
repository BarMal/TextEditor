package app.action.editor

import app.buffer.{BufferState, TogglingSet}
import app.Modifier

sealed trait NavigateEffect(modifiers: List[Modifier]) extends BufferEffect {

  def boundsCheck(state: BufferState): Boolean

  def moveCursor(state: BufferState): Int

  private def selectionChange(state: BufferState): TogglingSet[Int] =
    state.selected.offer(state.cursorPosition)

  def effect(state: BufferState): BufferState =
    BufferState(
      buffer = state.buffer,
      cursorPosition =
        if (boundsCheck(state)) moveCursor(state) else state.cursorPosition,
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected =
        if modifiers.contains(Modifier.Shift) then selectionChange(state)
        else TogglingSet.empty[Int],
      writeMode = state.writeMode,
      currentFormatting = state.currentFormatting,
      formattingMap = state.formattingMap
    )
}

object NavigateEffect {

  case class CursorLeft(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.cursorPosition - 1
    override def boundsCheck(state: BufferState): Boolean =
      moveCursor(state) >= 0
  }

  case class CursorRight(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.cursorPosition + 1
    override def boundsCheck(state: BufferState): Boolean =
      moveCursor(state) <= state.buffer.weight
  }

  case class CursorUp(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.cursorPosition - state.lineLength
    override def boundsCheck(state: BufferState): Boolean =
      0 <= moveCursor(state)
  }

  case class CursorDown(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.cursorPosition + state.lineLength
    override def boundsCheck(state: BufferState): Boolean =
      moveCursor(state) <= state.buffer.weight
  }

  case class CursorToEnd(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      Math.min(
        state.buffer.weight,
        (1 + (state.cursorPosition / state.lineLength)) * state.lineLength
      )
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToStart(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      Math.max(
        state.cursorPosition - (state.cursorPosition % state.lineLength),
        0
      )
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToTop(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int      = state.cursorPosition
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToBottom(modifiers: List[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int      = state.cursorPosition
    override def boundsCheck(state: BufferState): Boolean = true
  }

}
