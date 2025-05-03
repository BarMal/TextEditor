package app.render

import com.googlecode.lanterna.{SGR, TextCharacter}
import com.googlecode.lanterna.TextColor.ANSI

object Element {

  def fromString(
      in: String,
      lineLength: Int,
      foregroundColour: ANSI,
      background: ANSI,
      sgr: List[SGR] = List.empty[SGR]
  ): List[Output] =
    in.zipWithIndex.map { case (c, i) =>
      Output(
        textCharacter = new TextCharacter(c, foregroundColour, background, sgr*),
        x = Math.floorMod(i, lineLength),
        y = Math.floorDiv(i, lineLength)
      )
    }.toList

}
