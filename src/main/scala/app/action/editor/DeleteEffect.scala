package app.action.editor

import app.State

sealed trait DeleteEffect extends BufferEffect {

  protected def deleteNLeft(state: State, n: Int): State = {
    val (left, right) = state.buffer.splitAt(state.cursorPosition)
    State(
      buffer = left.dropRight(n).append(right),
      cursorPosition = Math.max(state.cursorPosition - n, 0),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None
    )
  }

  protected def deleteNRight(state: State, n: Int): State = {
    val (left, right) = state.buffer.splitAt(state.cursorPosition)
    State(
      buffer = left.append(right.drop(n)),
      cursorPosition =
        Math.min(state.cursorPosition + n, state.buffer.length()),
      userEffects = this :: state.userEffects,
      lineLength = state.lineLength,
      selected = None
    )
  }

}

object DeleteEffect {

  case object DeleteLeft extends DeleteEffect {

    override def effect: State => State = state =>
      deleteNLeft(state, state.selected.map(_.length).getOrElse(1))
  }

  case object DeleteRight extends DeleteEffect {
    override def effect: State => State = state =>
      deleteNRight(state, state.selected.map(_.length).getOrElse(1))
  }

}
