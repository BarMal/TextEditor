package app.render

import app.action.Effect
import app.buffer.rope.Rope
import app.buffer.TogglingSet
import com.googlecode.lanterna.{SGR, TextCharacter}
import com.googlecode.lanterna.TextColor.ANSI

object Header {

  def fromState(
      buffer: Rope,
      cursorPosition: Int,
      selected: TogglingSet[Int],
      lineLength: Int,
      effects: List[Effect]
  ): List[Output] = {
    val cursorPositionElement = Element.fromString(
      s"""CursorPosition: ${Math.floorDiv(cursorPosition, lineLength)}, ${Math
          .floorMod(cursorPosition, lineLength)}""",
      lineLength,
      ANSI.YELLOW,
      ANSI.BLUE,
      List(SGR.BOLD)
    )

    val bufferSizeElement = Element.fromString(
      s"""Buffer size: ${buffer.weight}""",
      lineLength,
      ANSI.BLACK,
      ANSI.GREEN,
      List(SGR.ITALIC)
    )

    val lastEffectElement = Element.fromString(
      s"""Last effect: ${effects.headOption.getOrElse("N/A")}""".stripMargin,
      lineLength,
      ANSI.WHITE,
      ANSI.RED
    )

    val selectedRangeElement = Element.fromString(
      s"""Selected range: ${selected.toString}""",
      lineLength,
      ANSI.BLUE,
      ANSI.WHITE
    )

    List(
      selectedRangeElement,
      lastEffectElement,
      bufferSizeElement,
      cursorPositionElement
    ).foldLeft(List.empty[Output])((elem, acc) =>
      acc ++ elem.map(o =>
        o.copy(
          x = o.x + Math.floorMod(acc.length, lineLength),
          y = o.y + Math.floorDiv(acc.length, lineLength)
        )
      )
    )
  }
}
