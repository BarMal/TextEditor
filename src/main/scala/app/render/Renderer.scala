package app.render

import app.buffer.BufferState
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
    val body = Body.fromState(
      state.buffer,
      state.lineLength,
      state.selected,
      state.formattingMap,
      bodyRowOffset,
      bodyColumnOffset
    )
    val (x, y) = body
      .map(output => output.mappedIndex + 1 -> (output.x + 1, output.y))
      .toMap
      .getOrElse(state.cursorPosition, (bodyColumnOffset, bodyRowOffset))
    writer.print(header ++ body) *> writer.updateCursorPosition(x, y)
  }

}
