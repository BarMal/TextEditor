package app.buffer

import app.buffer.Rope.{Leaf, Node}

sealed trait Rope {
  def weight: Int

  def collect: String = {
    def _collect(curr: Rope, acc: List[String]): List[String] =
      curr match
        case Node(Some(left), Some(right)) =>
          _collect(left, acc) ::: _collect(right, acc)
        case Node(None, Some(right)) => _collect(right, acc)
        case Node(Some(left), None)  => _collect(left, acc)
        case Node(None, None)        => "" :: acc
        case Leaf(value)             => value :: acc

    _collect(this, List.empty[String]).mkString
  }
}

object Rope {

  def empty: Rope = Node(None, None)

  case class Node(left: Option[Rope], right: Option[Rope]) extends Rope:
    override def weight: Int = left
      .map {
        case Node(l, r) =>
          l.map(_.weight).getOrElse(0) + r.map(_.weight).getOrElse(0)
        case Leaf(value) => value.length
      }
      .getOrElse(0)

  case class Leaf(value: String) extends Rope:
    override def weight: Int = value.length
}
