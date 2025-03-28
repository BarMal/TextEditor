package app.action.editor

import app.{BufferState, WriteMode}

sealed trait WriteEffect extends BufferEffect {

  protected def write[T]: BufferState => T => BufferState = state =>
    in =>
      BufferState(
        buffer = {
          val (pre, post) = state.buffer.splitAt(state.cursorPosition)
          (pre + in) + post
        },
        cursorPosition = state.cursorPosition + 1,
        userEffects = this :: state.userEffects,
        lineLength = state.lineLength,
        selected = None,
        writeMode = state.writeMode
      )

  protected def overwrite[T]: BufferState => T => BufferState = state =>
    in =>
      BufferState(
        buffer = {
          val (pre, post) = state.buffer.splitAt(state.cursorPosition)
          (pre + in) + post.drop(1)
        },
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
