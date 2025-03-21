package app.action.editor

import app.State

sealed trait WriteEffect extends BufferEffect {
  def effect: State => State

  protected def write[T]: State => T => State = state =>
    in =>
      State(
        buffer = state.buffer.insert(state.cursorPosition, in),
        cursorPosition = state.cursorPosition + 1,
        userEffects = this :: state.userEffects,
        lineLength = state.lineLength,
        selected = None
      )
}

object WriteEffect {

  case class Write(char: Char) extends WriteEffect {

    override def effect: State => State = state => write(state)(char)

  }

  case object Return extends WriteEffect {
    override def effect: State => State = state => write(state)('\n')
  }

}
