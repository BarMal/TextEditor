package app.action

import app.UserInput
import cats.Show

trait Effect

object Effect {

  case object Escape extends Effect

  case class Unexpected(input: UserInput*) extends Effect

  given effectListShowInstance: Show[List[Effect]] =
    (t: List[Effect]) => t.take(5).mkString(", ")
}




