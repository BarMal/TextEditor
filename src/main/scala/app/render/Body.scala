package app.render

import app.buffer
import app.buffer.Formatting.*
import app.buffer.rope.Rope
import app.buffer.{BufferState, Formatting, TogglingSet}
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.{SGR, TerminalTextUtils, TextCharacter}

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

  case class OutputState(col: Int, row: Int, acc: List[Output])

  object OutputState {
    def apply(x: Int, y: Int): OutputState =
      OutputState(x, y, List.empty[Output])
  }

  def fromState(
      buffer: Rope,
      lineLength: Int,
      selected: TogglingSet[Int],
      formattingMap: Map[Int, Set[Formatting]],
      rowOffset: Int,
      columnOffset: Int
  ): List[Output] =
    buffer
      .collect()
      .zipWithIndex
      .foldLeft(OutputState(columnOffset, rowOffset)) {
        case (OutputState(col, row, acc), (char, index)) =>

          val isEndOfRow: Boolean =
            col != 0 && Math.floorMod(col, lineLength) == 0

          val selectedFormatting: List[SGR] =
            if selected.exists(index) then List(SGR.REVERSE)
            else List.empty[SGR]
          val (selectStart, selectEnd) = selected.range((0, 0))
          val _selectedFormatting: List[SGR] =
            if selectStart <= index && index <= selectEnd then List(SGR.REVERSE)
            else List.empty[SGR]

          val sgrs: List[SGR] = selectedFormatting ++ formattingMap
            .getOrElse(index, Set.empty[Formatting])
            .map {
              case Bold       => SGR.BOLD
              case Italic     => SGR.ITALIC
              case Underscore => SGR.UNDERLINE
              case Inverted   => SGR.REVERSE
            }
            .toList

          val outputs: List[Output] =
            if !TerminalTextUtils.isControlCharacter(char) then
              List(
                Output(
                  textCharacter = new TextCharacter(
                    char,
                    ANSI.WHITE_BRIGHT,
                    ANSI.BLACK_BRIGHT,
                    sgrs*
                  ),
                  x = col,
                  y = row,
                  mappedIndex = index
                )
              )
            else List.empty[Output]

          char match {
            case '\n' => OutputState(columnOffset, row + 1, acc)
            case '\t' => OutputState(col + 4, row, acc)
            case '\b' => OutputState(col, row, acc)
            case _ if isEndOfRow =>
              OutputState(columnOffset, row + 1, outputs ++ acc)
            case _ => OutputState(col + 1, row, outputs ++ acc)
          }
      }
      .acc

//  def fromState(
//      buffer: Rope,
//      lineLength: Int,
//      selected: TogglingSet[Int],
//      formattingMap: Map[Int, Formatting],
//      bodyRowOffset: Int,
//      bodyColumnOffset: Int
//  ): List[Output] =
//    buffer
//      .collect()
//      .zipWithIndex
//      .flatMap {
//        case (c, i) if !TerminalTextUtils.isControlCharacter(c) =>
//          List(
//            Output(
//              textCharacter = new TextCharacter(c),
//              x = Math.floorMod(i, lineLength) + bodyRowOffset,
//              y = Math.floorDiv(i, lineLength) + bodyColumnOffset
//            )
//          )
//        case (c, i) if TerminalTextUtils.isPrintableCharacter(c) =>
//          controlCharacterHandler(c, i)
//        case (c, i) =>
//          println(s"""$c at position $i was not printable""")
//          List.empty[Output]
//      }
//      .toList

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
