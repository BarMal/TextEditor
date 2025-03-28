package app.action.editor

import app.BufferState

sealed trait DeleteEffect extends BufferEffect {

  protected def deleteNLeft(state: BufferState, n: Int): BufferState = {
    val (left, right) = state.buffer.splitAt(state.cursorPosition)
    BufferState(
      buffer = left.dropRight(n) ++ right,
      cursorPosition = Math.max(state.cursorPosition - n, 0),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None,
      writeMode = state.writeMode
    )
  }

  protected def deleteNRight(state: BufferState, n: Int): BufferState = {
    val (left, right) = state.buffer.splitAt(state.cursorPosition)
    BufferState(
      buffer = left ++ right.drop(n),
      cursorPosition = Math.min(left.length, state.buffer.length()),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None,
      writeMode = state.writeMode
    )
  }

}

object DeleteEffect {

  case object DeleteLeft extends DeleteEffect {

    override def effect: BufferState => BufferState = state =>
      deleteNLeft(state, state.selected.map(_.length).getOrElse(1))
  }

  case object DeleteRight extends DeleteEffect {
    override def effect: BufferState => BufferState = state =>
      deleteNRight(state, state.selected.map(_.length).getOrElse(1))
  }

}
