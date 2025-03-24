package app.buffer

case class Leaf(value: String)(implicit balance: Balance) extends Rope:
  override def weight: Int               = value.length
  override def isWeightBalanced: Boolean = true
  override def isHeightBalanced: Boolean = true
