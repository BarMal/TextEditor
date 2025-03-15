package app

import UserAction.Action.*
import app.effect.Effect.Experimental
import cats.Show
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import effect.{BufferEffect, CursorOnlyEffect, Effect, RemoveEffect}

sealed abstract class UserAction(val raw: Int)

object UserAction {

  case class Printable(override val raw: Int)         extends UserAction(raw)
  case class Undefined(override val raw: Int)         extends UserAction(raw)
  sealed abstract class Action(override val raw: Int) extends UserAction(raw)

  object Action {
    case object Backspace extends Action(8)
    case object Escape    extends Action(27)
    case object Return    extends Action(13)

    def find(raw: Int): Option[Action] =
      all.find(_.raw == raw)

    private def all: Set[Action] = Sealerate.valuesOf[Action]
  }

  private def find(raw: Int): UserAction = raw match {
    case i if 32 <= i && i <= 126 => Printable(i)
    case i if 128 <= i            => Printable(i)
    case meta => Action.find(meta).getOrElse(Undefined(meta))
  }

  private def navHandler(actions: List[UserAction]) = actions match
    case Printable(65) :: _ => CursorOnlyEffect.UpArrow
    case Printable(66) :: _ => CursorOnlyEffect.DownArrow
    case Printable(67) :: _ => CursorOnlyEffect.RightArrow
    case Printable(68) :: _ => CursorOnlyEffect.LeftArrow
    case Printable(70) :: _ => CursorOnlyEffect.End
    case Printable(72) :: _ => CursorOnlyEffect.Home
    case _                  => Effect.Undefined(Printable(79) :: actions)

  private def navJumpHandler(actions: List[UserAction]) = actions match
    case Printable(51) :: _                   => RemoveEffect.Delete
    case Printable(53) :: Printable(126) :: _ => CursorOnlyEffect.PageUp
    case Printable(54) :: Printable(126) :: _ => CursorOnlyEffect.PageDown
    case _ => Effect.Undefined(Printable(91) :: actions)

  private def escapedHandler(actions: List[UserAction]) = actions match
    case Printable(79) :: arrows  => navHandler(arrows)
    case Printable(91) :: navJump => navJumpHandler(navJump)
    case Nil                      => Effect.Escape
    case _                        => Effect.Undefined(Escape :: actions)

  def keyStrokesToEffect(keyStroke: KeyStroke): Effect = {
    keyStroke
    Experimental(keyStroke)
  }

  def process(in: List[Int]): Effect =
    in.takeWhile(x => x != 0).map(find) match {
      case Printable(raw) :: Nil => BufferEffect.Write(raw.toChar)
      case Return :: _           => BufferEffect.Return
      case Backspace :: _        => RemoveEffect.Backspace
      case Escape :: escaped     => escapedHandler(escaped)
      case undefined             => Effect.Undefined(undefined)
    }

  given showInstance: Show[UserAction] = (t: UserAction) => t.toString

}
