package app.buffer

import scala.util.Try

case class Leaf(value: String)(using balance: Balance) extends Rope:
  override def weight: Int               = value.length
  override def isWeightBalanced: Boolean = true
  override def isHeightBalanced: Boolean = true
  override def rebalance: Rope =
    if (value.length > balance.leafChunkSize) Rope(value) else this
  override def split(index: Int): Option[(Rope, Rope)] =
    Some(Leaf(value.take(index)), Leaf(value.drop(index)))
  override def index(i: Int): Option[Char] = Try(value.charAt(i)).toOption
  override def insert(index: Int, char: Char): Rope = {
    val (pre, post) = value.splitAt(index)
    Rope((pre + char) + post).rebalance
  }
  override def delete(start: Int, end: Int): Rope = Leaf(
    value.take(start) + value.drop(end)
  )
  override def collect(): String                 = value
