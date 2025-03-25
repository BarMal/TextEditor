package app.terminal

import app.BufferState
import app.terminal.Renderable.{Body, Header, Spacer}
import cats.effect.kernel.Async

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

object Renderer {

  private val cursorBlinkInterval: FiniteDuration = 500.milliseconds

  private def cursorVisible(startTime: Long): Boolean =
    val elapsedTime = System.currentTimeMillis() - startTime
    Math.floorDiv(elapsedTime, cursorBlinkInterval.toMillis) % 2 == 0

  def render[F[_]: Async](
      writer: Writer[F],
      startTime: Long,
      state: BufferState
  ): F[Unit] =
    writer
      .print(
        Header(state).asElement,
        Spacer.asElement,
        Spacer.asElement,
        Body(state, cursorVisible(startTime)).asElement
      )

}
