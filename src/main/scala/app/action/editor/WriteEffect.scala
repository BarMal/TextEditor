package app.action.editor

import app.buffer.{BufferState, WriteMode}

sealed trait WriteEffect extends BufferEffect {

  protected def write: BufferState => Char => BufferState = state =>
    in =>
      BufferState(
        buffer = state.buffer.insert(state.cursorPosition, in),
        cursorPosition = state.cursorPosition + 1,
        userEffects = this :: state.userEffects,
        lineLength = state.lineLength,
        selected = None,
        writeMode = state.writeMode
      )

  protected def overwrite: BufferState => Char => BufferState = state =>
    in =>
      BufferState(
        buffer = state.buffer.replace(state.cursorPosition, in),
        cursorPosition = state.cursorPosition + 1,
        userEffects = this :: state.userEffects,
        lineLength = state.lineLength,
        selected = None,
        writeMode = state.writeMode
      )
}

object WriteEffect {

  case class TogglingWrite(char: Char) extends WriteEffect {
    override def effect: BufferState => BufferState = state =>
      state.writeMode match
        case WriteMode.Write     => write(state)(char)
        case WriteMode.Overwrite => overwrite(state)(char)
  }

  case object Return extends WriteEffect {
    override def effect: BufferState => BufferState = state =>
      write(state)('\n')
  }

  case object Tab extends WriteEffect {
    override def effect: BufferState => BufferState = state =>
      write(state)('\t')
  }

}
