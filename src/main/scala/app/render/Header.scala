package app.render

//import app.action.Effect
//import app.buffer.{BufferState, TogglingSet}
//import cats.Show
//import com.googlecode.lanterna.TextCharacter
//import com.googlecode.lanterna.TextColor.ANSI
import app.buffer.rope.Rope
import app.buffer.TogglingSet

object Header {

  def fromState(
      buffer: Rope,
      cursorPosition: Int,
      selected: TogglingSet[Int],
      lineLength: Int
  ): List[Output] =
    
    List.empty[Output]
}

//case class Header(
//    cursorPosition: (Int, Int),
//    bufferSize: Int,
//    lastEffect: Option[Effect],
//    selected: TogglingSet[Int]
//) extends Renderable:
//  def toTextCharacters: List[TextCharacter] =
//    Header.showInstance.show(this).map(char => new TextCharacter(char)).toList
//
//object Header {
//
//  private def cursorPositionFromState: BufferState => (Int, Int) = state =>
//    (
//      state.cursorPosition % state.lineLength,
//      Math.floorDiv(state.cursorPosition, state.lineLength)
//    )
//
//  def apply(state: BufferState): Header = Header(
//    cursorPosition = cursorPositionFromState(state),
//    bufferSize = state.buffer.weight,
//    lastEffect = state.userEffects.headOption,
//    selected = state.selected
//  )
//
//  given showInstance: Show[Header] =
//    (t: Header) =>
//      s"""Cursor position: ${t.cursorPosition._1}, ${t.cursorPosition._2} | Buffer size: ${t.bufferSize} | Last effect: ${t.lastEffect
//          .map(_.toString)
//          .getOrElse("N/A")} | Selected range: ${t.selected.toString}"""
//
//}
