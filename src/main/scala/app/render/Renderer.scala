package app.render

import app.buffer.BufferState
import app.screen.ScreenWriter
import cats.effect.kernel.Async
import cats.implicits.catsSyntaxApplyOps

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

object Renderer {

  private val cursorBlinkInterval: FiniteDuration = 500.milliseconds

  private def cursorVisible(startTime: Long): Boolean =
    val elapsedTime = System.currentTimeMillis() - startTime
    Math.floorDiv(elapsedTime, cursorBlinkInterval.toMillis) % 2 == 0

  private val bodyRowOffset: Int    = 2
  private val bodyColumnOffset: Int = 4

  def render[F[_]: Async](
      writer: ScreenWriter[F],
      startTime: Long,
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
    val (x, y) = body.reverse.zipWithIndex
      .map((output, index) => index + 1 -> (output.x + 1b, output.y))
      .toMap
      .getOrElse(state.cursorPosition, (bodyColumnOffset, bodyRowOffset))
    writer.print(header ++ body) *> writer.updateCursorPosition(x, y)
  }

}
