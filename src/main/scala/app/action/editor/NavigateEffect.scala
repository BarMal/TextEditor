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
    override def moveCursor(state: BufferState): Int = {
      // Get all line boundaries including start and end
      val boundaries = state.newLineIndices.union(Set(0, state.buffer.weight))

      // Find the start of the current line
      val currentLineStart = boundaries
        .filter(_ <= state.cursorPosition)
        .maxOption
        .getOrElse(0)

      // Calculate column position within current line
      val columnPos = state.cursorPosition - currentLineStart

      // Find the start of the previous line
      val prevLineStart = boundaries
        .filter(_ < currentLineStart)
        .maxOption
        .getOrElse(0)

      // Find the end of the previous line (start of current line - 1)
      // But handle the newline character itself
      val prevLineEnd = if (currentLineStart > 0) currentLineStart - 1 else 0

      // Calculate the length of the previous line
      val prevLineLength = prevLineEnd - prevLineStart

      // Move to the same column in the previous line, or end of line if shorter
      prevLineStart + Math.min(columnPos, prevLineLength)
    }

    override def boundsCheck(state: BufferState): Boolean = {
      val boundaries = state.newLineIndices.union(Set(0, state.buffer.weight))
      val currentLineStart = boundaries
        .filter(_ <= state.cursorPosition)
        .maxOption
        .getOrElse(0)

      // Check if there's a previous line
      boundaries.exists(_ < currentLineStart)
    }
  }

  case class CursorDown(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int = {
      // Get all line boundaries including start and end
      val boundaries = state.newLineIndices.union(Set(0, state.buffer.weight))

      // Find the start of the current line
      val currentLineStart = boundaries
        .filter(_ <= state.cursorPosition)
        .maxOption
        .getOrElse(0)

      // Calculate column position within current line
      val columnPos = state.cursorPosition - currentLineStart

      // Find the start of the next line (first boundary after current cursor)
      val nextLineStartOpt = boundaries
        .filter(_ > state.cursorPosition)
        .minOption

      nextLineStartOpt match {
        case Some(nextLineStart) =>
          // Find the end of the next line
          val nextLineEndOpt = boundaries
            .filter(_ > nextLineStart)
            .minOption

          val nextLineEnd = nextLineEndOpt.getOrElse(state.buffer.weight)

          // Calculate length of next line (excluding newline)
          val nextLineLength = nextLineEnd - nextLineStart -
            (if (nextLineEndOpt.isDefined) 1 else 0)

          // Move to same column or end of line if shorter
          nextLineStart + Math.min(columnPos, nextLineLength)

        case None =>
          // Already on last line, move to end
          state.buffer.weight
      }
    }

    override def boundsCheck(state: BufferState): Boolean = {
      val boundaries = state.newLineIndices.union(Set(0, state.buffer.weight))
      // Check if there's a next line
      boundaries.exists(_ > state.cursorPosition)
    }
  }

  case class CursorToEnd(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      // Find the next newline after cursor, or end of buffer
      state.newLineIndices
        .filter(_ > state.cursorPosition)
        .minOption
        .getOrElse(state.buffer.weight)
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToStart(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int =
      // Find the start of current line
      // This is either 0 or the position right after the previous newline
      state.newLineIndices
        .filter(_ < state.cursorPosition)
        .maxOption
        .map(_ + 1) // Move to position after the newline
        .getOrElse(0)

    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToTop(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int      = 0
    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToBottom(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int      = state.buffer.weight
    override def boundsCheck(state: BufferState): Boolean = true
  }

}
