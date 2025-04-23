package app.render

import app.buffer.BufferState
import app.screen.ScreenWriter
import cats.effect.kernel.Async
import com.googlecode.lanterna.TextCharacter
import com.googlecode.lanterna.TextColor.ANSI

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
      "Hello world!".zipWithIndex
        .map((c, i) => 
          Output(new TextCharacter(c, ANSI.WHITE, ANSI.BLACK), i, 0)).toList)
//        Body.fromState(
//          state.buffer,
//          state.cursorPosition,
//          cursorVisible(startTime),
//          state.lineLength,
//          state.selected,
//          state.formattingMap
//        )
    



}
