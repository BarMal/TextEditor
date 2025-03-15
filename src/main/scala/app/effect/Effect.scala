package app.effect

import app.UserAction
import cats.Show
import com.googlecode.lanterna.input.KeyStroke



trait Effect

object Effect {

  case object Escape extends Effect

  case class Undefined(raw: List[UserAction]) extends Effect

  case class Experimental(raw: KeyStroke) extends Effect
  
  given effectListShowInstance: Show[List[Effect]] =
    (t: List[Effect]) => t.take(5).mkString(", ")
}




