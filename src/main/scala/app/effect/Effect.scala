package app.effect

import app.UserInput
import cats.Show
import com.googlecode.lanterna.input.KeyStroke



trait Effect

object Effect {

  case object Escape extends Effect

  case class Unexpected(input: UserInput*) extends Effect

  given effectListShowInstance: Show[List[Effect]] =
    (t: List[Effect]) => t.take(5).mkString(", ")
}




