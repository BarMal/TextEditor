package app.action.editor

import app.Modifier
import app.buffer.{BufferState, TogglingSet}

sealed trait DeleteEffect extends BufferEffect {

  protected def deleteLeft(state: BufferState): BufferState =
    state.copy(
      buffer = state.buffer.deleteLeft(state.cursorPosition, 1),
      cursorPosition = Math.max(state.cursorPosition - 1, 0),
      userEffects = this +: state.userEffects,
      selected = TogglingSet.empty[Int]
    )

  protected def deleteRight(state: BufferState): BufferState =
    state.copy(
      buffer = state.buffer.deleteRight(state.cursorPosition, 1),
      cursorPosition = Math.min(state.cursorPosition, state.buffer.weight),
      userEffects = this +: state.userEffects,
      selected = TogglingSet.empty[Int]
    )

  protected def deleteRange(
      state: BufferState,
      start: Int,
      end: Int
  ): BufferState =
    state.copy(
      buffer = state.buffer.deleteRight(start, start + end),
      cursorPosition = start,
      userEffects = this +: state.userEffects,
      selected = TogglingSet.empty[Int]
    )

}

object DeleteEffect {

  case class DeleteLeft(modifiers: Vector[Modifier]) extends DeleteEffect {
    override def effect(state: BufferState): BufferState =
      val (start, end) =
        state.selected.range(
          (Math.max(0, state.cursorPosition - 1), state.cursorPosition)
        )
      deleteRange(state, start, end)
        .removeNewLineIndex(state.cursorPosition)
  }

  case class DeleteRight(modifiers: Vector[Modifier]) extends DeleteEffect {
    override def effect(state: BufferState): BufferState =
      val (start, end) =
        state.selected.range(
          (
            state.cursorPosition,
            Math.min(state.buffer.weight, state.cursorPosition + 1)
          )
        )
      deleteRange(state, start, end)
        .removeNewLineIndex(state.cursorPosition)
  }

}
