package app.action.editor

import app.{BufferState, WriteMode}
import app.action.Effect

sealed trait StateChangeEffect extends Effect {
  def effect: BufferState => BufferState
}

object StateChangeEffect {

  case object ToggleWriteMode extends StateChangeEffect {
    override def effect: BufferState => BufferState = state =>
      state.copy(writeMode = WriteMode.flip(state.writeMode))
  }

}
