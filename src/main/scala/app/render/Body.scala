package app.render

import app.BufferState
import cats.Show
import com.googlecode.lanterna.TextColor.ANSI

import scala.annotation.tailrec

case class Body(lines: List[String]) extends Renderable {
  override def asElement: Element =
    Element(Body.showInstance.show(this), ANSI.WHITE, ANSI.BLACK)
}

object Body {

  @tailrec
  private def lineBuilder(
      acc: List[String],
      in: String,
      lineLength: Int
  ): List[String] =
    if (in.isEmpty) acc.reverse
    else
      val maybeSliceEnd: Int = in.lastIndexOfSlice("\\s", lineLength)
      val sliceEnd     = if (maybeSliceEnd < 0) lineLength else maybeSliceEnd
      val (line, rest) = in.splitAt(sliceEnd)
      lineBuilder(line.mkString :: acc, rest, lineLength)

  private def insertCursor(
      state: BufferState,
      cursorCharacter: Char = '\u2588'
  ): String =
    state.buffer.replace(state.cursorPosition, cursorCharacter).collect()

  def apply(
      state: BufferState,
      isCursorVisible: Boolean,
      cursorCharacter: Char = '\u2588'
  ): Body =
    Body(
      lineBuilder(
        List.empty[String],
        if (isCursorVisible) insertCursor(state, cursorCharacter)
        else state.buffer.collect(),
        state.lineLength
      )
    )

  given showInstance: Show[Body] =
    (t: Body) => t.lines.mkString("\n\t")

}
