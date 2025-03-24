package app.buffer

case class Leaf(value: String)(using balance: Balance) extends Rope:
  override def weight: Int               = value.length
  override def isWeightBalanced: Boolean = true
  override def isHeightBalanced: Boolean = true
  override def split(index: Int): (Rope, Rope) = (Leaf(value.take(index)), Leaf(value.drop(index)))
  override def index(i: Int): Char = value.charAt(i)
