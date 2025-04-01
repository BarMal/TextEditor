package app.render

import cats.Show
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.TextColor.ANSI

import scala.language.implicitConversions

case class Element(
    repr: String,
    foregroundColour: ANSI,
    backgroundColour: ANSI
) {
  def padTo(i: Int): Element = this.copy(repr.padTo(i, ' '))
}

object Element {
  given showInstance: Show[Element] = (e: Element) => e.repr
}

trait Renderable {
  def asElement: Element
}

object Renderable {

  given showInstance: Show[Renderable] = {
    case header: Header => Header.showInstance.show(header)
    case _: Spacer      => "\n"
  }

}
