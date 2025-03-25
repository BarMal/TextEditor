package app.render

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.TextColor.ANSI

case class Spacer(foregroundColour: TextColor.ANSI = ANSI.WHITE, backgroundColour: TextColor.ANSI = ANSI.BLACK) extends Renderable:
  override def asElement: Element = Element("\n", foregroundColour, backgroundColour)
  
