package app.render

import app.buffer.{Formatting, TogglingSet}
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.{SGR, TextCharacter}

// Shared layout calculations that both rendering and cursor positioning can use
object LayoutEngine {

  case class LayoutPosition(
      screenCol: Int,
      screenRow: Int,
      bufferIndex: Int
  )

  case class LayoutState(
      screenCol: Int,
      screenRow: Int,
      positions: Vector[
        LayoutPosition
      ],
      outputs: Vector[Output]
  )

  private object LayoutState {
    def apply(startCol: Int, startRow: Int): LayoutState =
      LayoutState(
        startCol,
        startRow,
        Vector.empty[LayoutPosition],
        Vector.empty[Output]
      )
  }

  private def processCharacter(
      state: LayoutState,
      char: Char,
      bufferIndex: Int,
      lineLength: Int,
      columnOffset: Int,
      selected: Option[(Int, Int)] = None,
      formattingMap: Map[Int, Set[Formatting]] = Map.empty
  ): LayoutState = {

    val newPositions: Vector[LayoutPosition] =
      state.positions :+ LayoutPosition(
        state.screenCol,
        state.screenRow,
        bufferIndex
      )

    char match {
      case '\n' =>
        state.copy(
          screenCol = columnOffset,
          screenRow = state.screenRow + 1,
          positions = newPositions
        )

      case '\t' =>
        val newCol = state.screenCol + 4
        val (nextCol, nextRow) =
          if newCol - columnOffset >= lineLength then
            (columnOffset, state.screenRow + 1)
          else (newCol, state.screenRow)

        state.copy(
          screenCol = nextCol,
          screenRow = nextRow,
          positions = newPositions
        )

      case c if Character.isISOControl(c) =>
        state.copy(positions = newPositions)

      case c =>
        val isSelected = selected.exists { case (start, end) =>
          bufferIndex >= start && bufferIndex <= end
        }

        val selectedFormatting =
          if (isSelected) Vector(SGR.REVERSE) else Vector.empty[SGR]
        val customFormatting = formattingMap
          .getOrElse(bufferIndex, Set.empty[Formatting])
          .map {
            case Formatting.Bold       => SGR.BOLD
            case Formatting.Italic     => SGR.ITALIC
            case Formatting.Underscore => SGR.UNDERLINE
            case Formatting.Inverted   => SGR.REVERSE
          }
          .toVector

        val output =
          Output(
            textCharacter = new TextCharacter(
              c,
              ANSI.WHITE_BRIGHT,
              ANSI.BLACK_BRIGHT,
              selectedFormatting ++ customFormatting*
            ),
            x = state.screenCol,
            y = state.screenRow,
            mappedIndex = bufferIndex
          )

        val newCol = state.screenCol + 1
        val (nextCol, nextRow) = if (newCol - columnOffset >= lineLength) {
          (columnOffset, state.screenRow + 1)
        } else {
          (newCol, state.screenRow)
        }

        state.copy(
          screenCol = nextCol,
          screenRow = nextRow,
          positions = newPositions,
          outputs = state.outputs :+ output
        )
    }
  }

  def layout(
      content: String,
      lineLength: Int,
      rowOffset: Int,
      columnOffset: Int,
      selected: TogglingSet[Int] = TogglingSet.empty[Int],
      formattingMap: Map[Int, Set[Formatting]] = Map.empty
  ): LayoutState = content.zipWithIndex
    .foldLeft(LayoutState(columnOffset, rowOffset)) {
      case (state, (char, index)) =>
        processCharacter(
          state,
          char,
          index,
          lineLength,
          columnOffset,
          Option.unless(selected.isEmpty)(selected.range(0, 0)),
          formattingMap
        )
    }

  def cursorPosition(state: LayoutState, cursorPosition: Int): (Int, Int) =
    Option
      .when(
        cursorPosition < state.positions.length
          && state.positions
            .isDefinedAt(cursorPosition)
      )(state.positions(cursorPosition))
      .map(position => (position.screenCol, position.screenRow))
      .getOrElse((state.screenCol, state.screenRow))
}
