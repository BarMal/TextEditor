package app.buffer

import scala.util.Try

case class Leaf(value: String)(using balance: Balance) extends Rope:
  override def weight: Int               = value.length
  override def height: Int               = 1
  override def isWeightBalanced: Boolean = true
  override def isHeightBalanced: Boolean = true
  override def rebalance: Rope =
    if (value.length > balance.leafChunkSize) Rope(value) else this
  override def split(index: Int): Option[(Rope, Rope)] =
    Some(Leaf(value.take(index)), Leaf(value.drop(index)))
  override def index(i: Int): Option[Char] = Try(value.charAt(i)).toOption
  override def insert(index: Int, char: Char): Rope = {
    val (pre, post) = value.splitAt(index)
    Rope((pre + char) + post)
  }
  override def deleteLeft(start: Int, count: Int): Rope =
    val (pre, post) = value.splitAt(start)
    Leaf(pre.dropRight(count) + post)

  override def deleteRight(start: Int, count: Int): Rope =
    val (pre, post) = value.splitAt(start)
    Leaf(pre.take(start) + post.drop(count))

  override def collect(): String = value
