package app.action.editor

import app.buffer.{BufferState, TogglingSet}
import app.Modifier

sealed trait NavigateEffect(modifiers: Vector[Modifier]) extends BufferEffect {

  def boundsCheck(state: BufferState): Boolean

  def moveCursor(state: BufferState): Int

  private def selectionChange(state: BufferState): TogglingSet[Int] =
    state.selected.offer(state.cursorPosition)

  def effect(state: BufferState): BufferState =
    state.copy(
      cursorPosition =
        if (boundsCheck(state)) moveCursor(state) else state.cursorPosition,
      userEffects = this +: state.userEffects,
      selected =
        if modifiers.contains(Modifier.Shift) then selectionChange(state)
        else TogglingSet.empty[Int]
    )
}

object NavigateEffect {

  case class CursorLeft(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.cursorPosition - 1
    override def boundsCheck(state: BufferState): Boolean =
      moveCursor(state) >= 0
  }

  case class CursorRight(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.cursorPosition + 1
    override def boundsCheck(state: BufferState): Boolean =
      moveCursor(state) <= state.buffer.weight
  }

  case class CursorUp(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.newLineIndices
        .maxBefore(state.cursorPosition)
        .flatMap { startOfLine =>
          val columnPos = state.cursorPosition - startOfLine
          state.newLineIndices.maxBefore(startOfLine).map { startOfPrev =>
            Math.min(startOfLine + columnPos, startOfLine - 1)
          }
        }
        .getOrElse(0)

    override def boundsCheck(state: BufferState): Boolean =
      0 <= moveCursor(state)
  }

  case class CursorDown(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.newLineIndices
        .maxBefore(state.cursorPosition)
        .flatMap { startOfLine =>
          val columnPos = state.cursorPosition - startOfLine
          state.newLineIndices.minAfter(state.cursorPosition).map {
            startOfNext =>
              Math.min(startOfNext + columnPos, startOfNext + 1)
          }
        }
        .getOrElse(state.buffer.weight)

    override def boundsCheck(state: BufferState): Boolean =
      moveCursor(state) <= state.buffer.weight
  }

  case class CursorToEnd(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.newLineIndices
        .minAfter(state.cursorPosition)
        .getOrElse(state.buffer.weight)
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToStart(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      state.newLineIndices.maxBefore(state.cursorPosition).getOrElse(0)

    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToTop(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int      = state.cursorPosition
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToBottom(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int      = state.cursorPosition
    override def boundsCheck(state: BufferState): Boolean = true
  }

}
