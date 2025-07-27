package app.render

import app.buffer.BufferState
import app.render.renderable.{Body, Header}
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

    val (body, (cursorX, cursorY)) = LayoutEngine.layoutContent(
      content = state.buffer
        .slice(
          state.cursorPosition - state.viewportSize / 2,
          state.cursorPosition + state.viewportSize / 2
        )
        .collect(),
      lineLength = state.lineLength,
      rowOffset = bodyRowOffset,
      columnOffset = bodyColumnOffset,
      cursorPosition = state.cursorPosition,
      selected = state.selected,
      formattingMap = state.formattingMap
    )

    writer
      .print(header.appendedAll(body)) *> writer.updateCursorPosition(
      cursorX,
      cursorY
    )
  }
}
