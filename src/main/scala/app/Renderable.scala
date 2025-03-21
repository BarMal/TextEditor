package app

import app.action.Effect
import cats.Show

import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait Renderable

object Renderable {

  given showInstance: Show[Renderable] = {
    case header: Header => Header.showInstance.show(header)
    case body: Body     => Body.showInstance.show(body)
  }

  case class Header(
      cursorPosition: (Int, Int),
      bufferSize: Int,
      lastEffect: Option[Effect]
  ) extends Renderable

  object Header {

    private def cursorPositionFromState: BufferState => (Int, Int) = state =>
      (
        state.cursorPosition % state.lineLength,
        Math.floorDiv(state.cursorPosition, state.lineLength)
      )

    def apply(state: BufferState): Header = Header(
      cursorPosition = cursorPositionFromState(state),
      bufferSize = state.buffer.length(),
      lastEffect = state.userEffects.headOption
    )

    given showInstance: Show[Header] =
      (t: Header) =>
        s"""Cursor position: ${t.cursorPosition._1}, ${t.cursorPosition._2} | Buffer size: ${t.bufferSize} | Last effect: ${t.lastEffect
            .map(_.toString)
            .getOrElse("N/A")}"""

  }

  case class Body(lines: List[String]) extends Renderable

  object Body {

    @tailrec
    private def lineBuilder(
        acc: List[String],
        in: StringBuilder,
        lineLength: Int
    ): List[String] =
      if (in.isEmpty) acc.reverse
      else
        val maybeSliceEnd: Int = in.lastIndexOfSlice("\\s", lineLength)
        val sliceEnd = if (maybeSliceEnd < 0) lineLength else maybeSliceEnd
        val (line, rest)  = in.splitAt(sliceEnd)
        lineBuilder(line.mkString :: acc, rest, lineLength)

    def apply(state: BufferState): Body =
      Body(lineBuilder(List.empty[String], state.buffer, state.lineLength))

    given showInstance: Show[Body] =
      (t: Body) => t.lines.map(line => s"\t$line").mkString("\n")

  }

}
