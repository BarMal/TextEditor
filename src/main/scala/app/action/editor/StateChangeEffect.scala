package app.action.editor

import app.action.Effect
import app.buffer.{BufferState, WriteMode}

sealed trait StateChangeEffect extends Effect {
  def effect: BufferState => BufferState
}

object StateChangeEffect {

  case object ToggleWriteMode extends StateChangeEffect {
    override def effect: BufferState => BufferState = state =>
      state.copy(writeMode = WriteMode.flip(state.writeMode))
  }

}
