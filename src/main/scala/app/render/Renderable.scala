package app.render

import com.googlecode.lanterna.TextCharacter

case class Output(textCharacter: TextCharacter, x: Int, y: Int, mappedIndex: Int)

trait Renderable {
  def toTextCharacters: List[Output]
}
