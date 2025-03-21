package app.action.editor

import app.State
import app.action.Effect

trait BufferEffect extends Effect {
  def effect: State => State
}
