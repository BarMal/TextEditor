package app.action.editor

import app.action.Effect
import app.buffer.BufferState

trait BufferEffect extends Effect {
  def effect: BufferState => BufferState
}
