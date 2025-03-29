package app.render

import app.BufferState
import app.action.Effect
import cats.Show
import com.googlecode.lanterna.TextColor.ANSI

case class Header(
    cursorPosition: (Int, Int),
    bufferSize: Int,
    lastEffect: Option[Effect]
) extends Renderable:
  override def asElement: Element =
    Element(Header.showInstance.show(this), ANSI.WHITE, ANSI.RED)

object Header {

  private def cursorPositionFromState: BufferState => (Int, Int) = state =>
    (
      state.cursorPosition % state.lineLength,
      Math.floorDiv(state.cursorPosition, state.lineLength)
    )

  def apply(state: BufferState): Header = Header(
    cursorPosition = cursorPositionFromState(state),
    bufferSize = state.buffer.weight,
    lastEffect = state.userEffects.headOption
  )

  given showInstance: Show[Header] =
    (t: Header) =>
      s"""Cursor position: ${t.cursorPosition._1}, ${t.cursorPosition._2} | Buffer size: ${t.bufferSize} | Last effect: ${t.lastEffect
          .map(_.toString)
          .getOrElse("N/A")}"""

}
