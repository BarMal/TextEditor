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

  override def rebalance: Rope =
    if isWeightBalanced then this
    else if left.mapOr(_.weight)(0) < right.mapOr(_.weight)(0) then rotateLeft()
    else rotateRight()

  override def split(index: Int): (Option[Rope], Option[Rope]) =
    if index == 0 then (None, Some(this))
    else if weight > index then {
      left match
        case Some(l) =>
          val (first, second) = l.split(index)
          (
            first.map(_.rebalance),
            second.map(r => Node(second, right).rebalance)
          )
        case None => (None, None)
    } else if weight < index then {
      right match
        case Some(r) =>
          val (first, second) =
            r.split(left.mapOr(l => index - l.weight)(0))
          (first.map(l => Node(left, first).rebalance), second.map(_.rebalance))
        case None => (None, None)
    } else (left, right)

  override def index(index: Int): Option[Char] =
    if weight > index then left.flatMap(_.index(index))
    else right.flatMap(_.index(index - weight))

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

object Node {
  def apply(left: Rope)(using balance: Balance): Node =
    Node(Some(left), None)

  def apply(left: Rope, right: Rope)(using balance: Balance): Node =
    Node(Some(left), Some(right))
}
