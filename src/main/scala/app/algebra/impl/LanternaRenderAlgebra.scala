package app.algebra.impl

import app.algebra.RenderAlgebra
import app.buffer.BufferState
import app.buffer.rope.Rope
import app.render.renderable.Header
import app.render.LayoutEngine
import app.screen.ScreenWriter
import cats.Apply.nonInheritedOps.toApplyOps
import cats.effect.Sync
import cats.syntax.functor.*

class LanternaRenderAlgebra[F[_]: Sync](writer: ScreenWriter[F]) extends RenderAlgebra[F] {
  private val bodyRowOffset: Int = 2
  private val bodyColumnOffset: Int = 4

  def render(state: BufferState): F[Unit] = {
    val header = Header.fromState(
      state.buffer,
      state.cursorPosition,
      state.selected,
      state.lineLength,
      state.userEffects
    )

    val bufferSlice: Rope = state.buffer
      .slice(
        state.cursorPosition - state.viewportSize / 2,
        state.cursorPosition + state.viewportSize / 2
      )

    val body: LayoutEngine.LayoutState = LayoutEngine.layout(
      content = bufferSlice.collect(),
      lineLength = state.lineLength,
      rowOffset = bodyRowOffset,
      columnOffset = bodyColumnOffset,
      selected = state.selected,
      formattingMap = state.formattingMap
    )

    val (cursorX, cursorY) = LayoutEngine.cursorPosition(
      state = body,
      cursorPosition = state.cursorPosition
    )

    writer.print(header.appendedAll(body.outputs)) *>
    updateCursorPosition(cursorX, cursorY)
  }

  def updateCursorPosition(x: Int, y: Int): F[Unit] =
    writer.updateCursorPosition(x, y)
}
