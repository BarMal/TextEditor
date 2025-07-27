package app.render.renderable

import app.render.Output
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.{SGR, TextCharacter}

object Element {

  def fromString(
      in: String,
      lineLength: Int,
      foregroundColour: ANSI,
      background: ANSI,
      sgr: List[SGR] = List.empty[SGR]
  ): Vector[Output] =
    in.zipWithIndex.map { case (c, i) =>
      Output(
        textCharacter =
          new TextCharacter(c, foregroundColour, background, sgr*),
        x = Math.floorMod(i, lineLength),
        y = Math.floorDiv(i, lineLength),
        mappedIndex = i
      )
    }.toVector

}
