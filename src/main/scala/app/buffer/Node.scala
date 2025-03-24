package app.buffer

case class Node(left: Option[Rope], right: Option[Rope])(implicit
    balance: Balance
) extends Rope:

  override def weight: Int = {
    def subtreeWeight(rope: Option[Rope]): Int = rope match
      case Some(Node(l, r))  => subtreeWeight(l) + subtreeWeight(r)
      case Some(Leaf(value)) => value.length
      case _                 => 0

    subtreeWeight(left)
  }

  def rebalance(): Node =
    if isWeightBalanced then this
    else if (left.mapOr(_.weight)(0) < right.mapOr(_.weight)(0)) this
    else this

  override def isWeightBalanced: Boolean = Math.abs(
    left.mapOr(_.weight)(0) - right.mapOr(_.weight)(0)
  ) <= balance.weightBalance

  private def height(node: Option[Rope]): Int =
    node.mapOr {
      case Node(l, r) => Math.max(height(l), height(r)) + 1
      case Leaf(_)    => 1
    }(0)

  override def isHeightBalanced: Boolean =
    Math.abs(height(left) - height(right)) <= balance.heightBalance

object Node {
  def apply(left: Rope)(implicit balance: Balance): Node =
    Node(Some(left), None)

  def apply(left: Rope, right: Rope)(implicit balance: Balance): Node =
    Node(Some(left), Some(right))
}
