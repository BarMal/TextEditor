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
      val content = state.buffer.collect()

      // Find current line boundaries
      val textBefore = content.take(state.cursorPosition)
      val currentLineStart = textBefore.lastIndexOf('\n') match {
        case -1  => 0
        case idx => idx + 1
      }

      // If we're at the start of the buffer, can't go up
      if (currentLineStart == 0 && state.cursorPosition == 0) {
        return state.cursorPosition
      }

      // Column position in current line
      val columnPos = state.cursorPosition - currentLineStart

      // Find previous line start
      val beforeCurrentLine = content.take(currentLineStart)
      val prevLineStart = if (currentLineStart > 0) {
        beforeCurrentLine.dropRight(1).lastIndexOf('\n') match {
          case -1  => 0
          case idx => idx + 1
        }
      } else 0

      // Find previous line end (excluding the newline)
      val prevLineEnd = if (currentLineStart > 0) currentLineStart - 1 else 0

      // Length of previous line
      val prevLineLength = prevLineEnd - prevLineStart

      // Move to same column in previous line, or end if shorter
      prevLineStart + Math.min(columnPos, prevLineLength)
    }

    override def boundsCheck(state: BufferState): Boolean = {
      val content    = state.buffer.collect()
      val textBefore = content.take(state.cursorPosition)
      val currentLineStart = textBefore.lastIndexOf('\n') match {
        case -1  => 0
        case idx => idx + 1
      }

      // Can go up if we're not on the first line
      currentLineStart > 0
    }
  }

  case class CursorDown(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int = {
      val content = state.buffer.collect()

      // Find current line boundaries
      val textBefore = content.take(state.cursorPosition)
      val currentLineStart = textBefore.lastIndexOf('\n') match {
        case -1  => 0
        case idx => idx + 1
      }

      // Column position in current line
      val columnPos = state.cursorPosition - currentLineStart

      // Find next line start
      val textAfter = content.drop(state.cursorPosition)
      val nextLineStartOffset = textAfter.indexOf('\n') match {
        case -1  => return state.cursorPosition // No next line
        case idx => idx + 1
      }

      val nextLineStart = state.cursorPosition + nextLineStartOffset

      // If at end of buffer, can't go down
      if (nextLineStart >= content.length) {
        return state.cursorPosition
      }

      // Find next line end
      val textFromNextLine = content.drop(nextLineStart)
      val nextLineEnd = textFromNextLine.indexOf('\n') match {
        case -1  => content.length
        case idx => nextLineStart + idx
      }

      // Length of next line
      val nextLineLength = nextLineEnd - nextLineStart

      // Move to same column in next line, or end if shorter
      nextLineStart + Math.min(columnPos, nextLineLength)
    }

    override def boundsCheck(state: BufferState): Boolean = {
      val content   = state.buffer.collect()
      val textAfter = content.drop(state.cursorPosition)

      // Can go down if there's a newline after cursor
      textAfter.indexOf('\n') >= 0
    }
  }

  case class CursorToEnd(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int = {
      val content   = state.buffer.collect()
      val textAfter = content.drop(state.cursorPosition)

      textAfter.indexOf('\n') match {
        case -1  => state.buffer.weight        // End of buffer
        case idx => state.cursorPosition + idx // End of current line
      }
    }

    override def boundsCheck(state: BufferState): Boolean = true
  }

  case class CursorToStart(modifiers: Vector[Modifier])
      extends NavigateEffect(modifiers) {
    override def moveCursor(state: BufferState): Int = {
      val content    = state.buffer.collect()
      val textBefore = content.take(state.cursorPosition)

      textBefore.lastIndexOf('\n') match {
        case -1  => 0       // Start of buffer
        case idx => idx + 1 // Start of current line (after the newline)
      }
    }

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
