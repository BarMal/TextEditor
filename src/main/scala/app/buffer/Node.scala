package app.buffer

case class Node(left: Option[Rope], right: Option[Rope])(using balance: Balance)
    extends Rope:

  override def weight: Int = {
    def subtreeWeight(rope: Option[Rope]): Int = rope match
      case Some(Node(l, r))  => subtreeWeight(l) + subtreeWeight(r)
      case Some(Leaf(value)) => value.length
      case _                 => 0

    subtreeWeight(left)
  }

  override def isHeightBalanced: Boolean =
    Math.abs(height(left) - height(right)) <= balance.heightBalance

  override def isWeightBalanced: Boolean = Math.abs(
    left.mapOr(_.weight)(0) - right.mapOr(_.weight)(0)
  ) <= balance.weightBalance

  def rebalance: Node =
    if isWeightBalanced then this
    else if left.mapOr(_.weight)(0) < right.mapOr(_.weight)(0) then rotateLeft()
    else rotateRight()

  private def rotateLeft(): Node = right
    .map {
      case Node(l, r) => Node(Some(Node(left, l)), r)
      case Leaf(_)    => this
    }
    .getOrElse(this)

  private def rotateRight(): Node = left
    .map {
      case Node(l, r) => Node(l, Some(Node(r, right)))
      case Leaf(_)    => this
    }
    .getOrElse(this)

  private def height(node: Option[Rope]): Int =
    node.mapOr {
      case Node(l, r) => Math.max(height(l), height(r)) + 1
      case Leaf(_)    => 1
    }(0)

  override def indexOf(i: Int): Option[Char] = {
    if weight > i then
      left.flatMap(r => r.indexOf(i))
    else right.flatMap(r => r.indexOf(i - weight))
  }

  override def split(index: Int): (Rope, Rope) = ???

object Node {
  def apply(left: Rope)(using balance: Balance): Node =
    Node(Some(left), None)

  def apply(left: Rope, right: Rope)(using balance: Balance): Node =
    Node(Some(left), Some(right))
}
