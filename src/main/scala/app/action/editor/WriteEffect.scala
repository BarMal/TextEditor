package app.action.editor

import app.buffer.{BufferState, TogglingSet, WriteMode}

sealed trait WriteEffect extends BufferEffect {

  protected def write(state: BufferState, in: Char): BufferState =
    state.copy(
      buffer = state.buffer.insert(state.cursorPosition, in),
      cursorPosition = state.cursorPosition + 1,
      userEffects = this +: state.userEffects,
      selected = TogglingSet.empty[Int]
    )

  protected def overwrite(state: BufferState, in: Char): BufferState =
    state.copy(
      buffer = state.buffer.replace(state.cursorPosition, in),
      cursorPosition = state.cursorPosition + 1,
      userEffects = this +: state.userEffects,
      selected = TogglingSet.empty[Int]
    )
}

object WriteEffect {

  case class TogglingWrite(char: Char) extends WriteEffect {
    override def effect(state: BufferState): BufferState =
      state.writeMode match
        case WriteMode.Write     => write(state, char)
        case WriteMode.Overwrite => overwrite(state, char)
  }

  case object Return extends WriteEffect {
    override def effect(state: BufferState): BufferState =
      write(state, '\n').addNewLineIndex(state.cursorPosition)
  }

  case object Tab extends WriteEffect {
    override def effect(state: BufferState): BufferState =
      write(state, '\t')
  }

}
