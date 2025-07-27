package app.render

import app.buffer.BufferState
import app.buffer.rope.Rope
import app.render.renderable.Header
import app.screen.ScreenWriter
import cats.effect.kernel.Async
import cats.implicits.catsSyntaxApplyOps

import scala.language.postfixOps

object Renderer {

  private val bodyRowOffset: Int    = 2
  private val bodyColumnOffset: Int = 4

  def render[F[_]: Async](
      writer: ScreenWriter[F],
      state: BufferState
  ): F[Unit] = {
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

    writer
      .print(header.appendedAll(body.outputs))
      *> writer.updateCursorPosition(cursorX, cursorY)
  }
}
