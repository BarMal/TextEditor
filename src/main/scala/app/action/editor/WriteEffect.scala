package app.action.editor

import app.BufferState

sealed trait WriteEffect extends BufferEffect {
  def effect: BufferState => BufferState

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
        selected = None
      )
}

object WriteEffect {

  case class Write(char: Char) extends WriteEffect {

    override def effect: BufferState => BufferState = state => write(state)(char)

  }

  case object Return extends WriteEffect {
    override def effect: BufferState => BufferState = state => write(state)('\n')
  }

}
