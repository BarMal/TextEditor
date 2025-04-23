package app.render

import app.buffer.rope.Rope
import app.buffer.{BufferState, Formatting, TogglingSet}
import com.googlecode.lanterna.TextCharacter

//
//import app.buffer.BufferState
//import cats.Show
//import com.googlecode.lanterna.TextColor.ANSI
//
//import scala.annotation.tailrec
//
////case class Body(lines: List[String]) extends Renderable {
////  override def asElement: Element =
////    Element(Body.showInstance.show(this), ANSI.WHITE, ANSI.BLACK)
////}
////
////object Body {
////
////  @tailrec
////  private def lineBuilder(
////      acc: List[String],
////      in: String,
////      lineLength: Int
////  ): List[String] =
////    if (in.isEmpty) acc.reverse
////    else
////      val maybeSliceEnd: Int = in.lastIndexOfSlice("\\s", lineLength)
////      val sliceEnd     = if (maybeSliceEnd < 0) lineLength else maybeSliceEnd
////      val (line, rest) = in.splitAt(sliceEnd)
////      lineBuilder(line.mkString :: acc, rest, lineLength)
////
////  private def insertCursor(
////      state: BufferState,
////      cursorCharacter: Char = '\u2588'
////  ): String =
////    state.buffer.replace(state.cursorPosition, cursorCharacter).collect()
////
////  def apply(
////      state: BufferState,
////      isCursorVisible: Boolean,
////      cursorCharacter: Char = '\u2588'
////  ): Body =
////    Body(
////      lineBuilder(
////        List.empty[String],
////        if (isCursorVisible) insertCursor(state, cursorCharacter)
////        else state.buffer.collect(),
////        state.lineLength
////      )
////    )
////
////  given showInstance: Show[Body] =
////    (t: Body) => t.lines.map(s => s"""\n\t$s""").mkString
////
////}
//
object Body {

  def fromState(
      buffer: Rope,
      cursorPosition: Int,
      cursorVisible: Boolean,
      lineLength: Int,
      selected: TogglingSet[Int],
      formattingMap: Map[Int, Formatting]
  ): List[TextCharacter] = {
    val rope: Rope =
      if cursorVisible then buffer.replace(cursorPosition, '\u2588') else buffer

    List.empty[TextCharacter]
  }

}
//  def foo(
//      state: BufferState,
//      isCursorVisible: Boolean,
//      cursorCharacter: Char = '\u2588'
//  ): List[Nothing] = {
//    val rope: BufferState =
//      if isCursorVisible then insertCursor(state, cursorCharacter) else state
//
//    rope.selected match
//      case Some(selectedRange) =>
//        val startIndex = Math.min(selectedRange.start, selectedRange.end)
//        val endIndex = Math.max(selectedRange.start, selectedRange.end)
//        (for {
//          startSplit <- rope.buffer.splitAt(startIndex)
//          (start, rest) = startSplit
//          endSplit <- rest.splitAt(endIndex)
//          (highlighted, end) = endSplit
//        } yield List(
//          Element(start.collect(), ANSI.WHITE, ANSI.BLACK),
//          Element(highlighted.collect(), ANSI.BLACK, ANSI.WHITE),
//          Element(end.collect(), ANSI.WHITE, ANSI.BLACK)
//        )).getOrElse(
//          List(Element(rope.buffer.collect(), ANSI.WHITE, ANSI.BLACK))
//        )
//      case None => List(Element(rope.buffer.collect(), ANSI.WHITE, ANSI.BLACK))
//  }
//
//  @tailrec
//  private def lineBuilder(
//      in: String,
//      lineLength: Int,
//      acc: List[String] = Nil
//  ): List[String] =
//    if (in.isEmpty) acc.reverse
//    else
//      val maybeSliceEnd: Int = in.lastIndexOfSlice("\\s", lineLength)
//      val sliceEnd     = if (maybeSliceEnd < 0) lineLength else maybeSliceEnd
//      val (line, rest) = in.splitAt(sliceEnd)
//      lineBuilder(rest, lineLength, line.mkString :: acc)
//
//  private def insertCursor(
//      state: BufferState,
//      cursorCharacter: Char = '\u2588'
//  ): BufferState =
//    state.copy(buffer =
//      state.buffer.replace(state.cursorPosition, cursorCharacter)
//    )
//
//}
