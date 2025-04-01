package app.action.editor

import app.BufferState

sealed trait DeleteEffect extends BufferEffect {

  protected def deleteLeft(state: BufferState): BufferState =
    BufferState(
      buffer =
        state.buffer.deleteLeft(state.cursorPosition, 1),
      cursorPosition = Math.max(state.cursorPosition - 1, 0),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None,
      writeMode = state.writeMode
    )

  protected def deleteNLeft(state: BufferState, n: Int): BufferState =
    BufferState(
      buffer =
        state.buffer.deleteLeft(state.cursorPosition, n),
      cursorPosition = Math.max(state.cursorPosition - n, 0),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None,
      writeMode = state.writeMode
    )

  protected def deleteRight(state: BufferState): BufferState =
    BufferState(
      buffer = state.buffer.deleteRight(state.cursorPosition, 1),
      cursorPosition = Math.min(state.cursorPosition, state.buffer.weight),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None,
      writeMode = state.writeMode
    )

  protected def deleteNRight(state: BufferState, n: Int): BufferState =
    BufferState(
      buffer = state.buffer.deleteRight(state.cursorPosition, n),
      cursorPosition = Math.min(state.cursorPosition, state.buffer.weight),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None,
      writeMode = state.writeMode
    )

}

object DeleteEffect {

  case object DeleteLeft extends DeleteEffect {

    override def effect: BufferState => BufferState = state =>
      state.selected match
        case Some(selected) if selected.start < selected.end =>
          deleteNLeft(state, Math.abs(selected.start - selected.end))
        case Some(selected) => deleteNRight(state, Math.abs(selected.start - selected.end))
        case None => deleteLeft(state)
  }

  case object DeleteRight extends DeleteEffect {
    override def effect: BufferState => BufferState = state =>
      state.selected match
        case Some(selected) if selected.start < selected.end =>
          deleteNLeft(state, Math.abs(selected.start - selected.end))
        case Some(selected) => deleteNRight(state, Math.abs(selected.start - selected.end))
        case None => deleteRight(state)
  }

}
