package app.action.editor

import app.BufferState
import app.action.Effect

trait BufferEffect extends Effect {
  def effect: BufferState => BufferState
}
