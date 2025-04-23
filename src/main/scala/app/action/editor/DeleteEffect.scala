package app.action.editor

import app.Modifier
import app.buffer.{BufferState, TogglingSet}

sealed trait DeleteEffect extends BufferEffect {

  protected def deleteLeft(state: BufferState): BufferState =
    BufferState(
      buffer = state.buffer.deleteLeft(state.cursorPosition, 1),
      cursorPosition = Math.max(state.cursorPosition - 1, 0),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = TogglingSet.empty[Int],
      writeMode = state.writeMode,
      currentFormatting = state.currentFormatting,
      formattingMap = state.formattingMap
    )

  protected def deleteNLeft(state: BufferState, n: Int): BufferState =
    BufferState(
      buffer = state.buffer.deleteLeft(state.cursorPosition, n),
      cursorPosition = Math.max(state.cursorPosition - n, 0),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = TogglingSet.empty[Int],
      writeMode = state.writeMode,
      currentFormatting = state.currentFormatting,
      formattingMap = state.formattingMap
    )

  protected def deleteRight(state: BufferState): BufferState =
    BufferState(
      buffer = state.buffer.deleteRight(state.cursorPosition, 1),
      cursorPosition = Math.min(state.cursorPosition, state.buffer.weight),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = TogglingSet.empty[Int],
      writeMode = state.writeMode,
      currentFormatting = state.currentFormatting,
      formattingMap = state.formattingMap
    )

  protected def deleteNRight(state: BufferState, n: Int): BufferState =
    BufferState(
      buffer = state.buffer.deleteRight(state.cursorPosition, n),
      cursorPosition = Math.min(state.cursorPosition, state.buffer.weight),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = TogglingSet.empty[Int],
      writeMode = state.writeMode,
      currentFormatting = state.currentFormatting,
      formattingMap = state.formattingMap
    )

  protected def deleteWordLeft(state: BufferState): BufferState =
    BufferState(
      buffer = state.buffer.deleteLeft(state.cursorPosition, 1),
      cursorPosition = Math.min(state.cursorPosition, state.buffer.weight),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = TogglingSet.empty[Int],
      writeMode = state.writeMode,
      currentFormatting = state.currentFormatting,
      formattingMap = state.formattingMap
    )

}

object DeleteEffect {

  case class DeleteLeft(modifiers: List[Modifier]) extends DeleteEffect {

    override def effect(state: BufferState): BufferState =
      if state.selected.isEmpty then deleteLeft(state)
      else {
        val (start, end) = state.selected.range
        deleteNLeft(state, start - end)
      }
  }

  case class DeleteRight(modifiers: List[Modifier]) extends DeleteEffect {
    override def effect(state: BufferState): BufferState =
      if state.selected.isEmpty then deleteRight(state)
      else {
        val (start, end) = state.selected.range
        deleteNRight(state, start - end)
      }
  }

}
