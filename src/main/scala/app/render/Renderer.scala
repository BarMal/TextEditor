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

  def render[F[_]: Async](
      writer: ScreenWriter[F],
      startTime: Long,
      state: BufferState
  ): F[Unit] =
    writer.print(
      Body.fromState(
        state.buffer,
        state.cursorPosition,
        cursorVisible(startTime),
        state.lineLength,
        state.selected,
        state.formattingMap
      )
    ) *> writer.updateCursorPosition(
      x = Math.floorMod(state.cursorPosition, state.lineLength),
      y = Math.floorDiv(state.cursorPosition, state.lineLength)
    )

}
