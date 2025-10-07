package app.algebra

import app.buffer.BufferState
import app.screen.ScreenWriter
import cats.Monad

/**
 * Algebra for rendering operations
 */
trait RenderAlgebra[F[_]] {
  def render(state: BufferState): F[Unit]
  def updateCursorPosition(x: Int, y: Int): F[Unit]
}
