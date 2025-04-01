package app.buffer

case class Node(left: Rope, right: Rope)(using balance: Balance) extends Rope:

  override def weight: Int = subtreeWeight(left)

  override def isWeightBalanced: Boolean =
    Math.abs(left.weight - right.weight) <= balance.weightBalance

  override def isHeightBalanced: Boolean =
    Math.abs(height(left) - height(right)) <= balance.heightBalance

  override def rebalance: Rope = if isWeightBalanced then this
  else if left.weight < right.weight then rotateLeft()
  else rotateRight()

  override def split(index: Int): Option[(Rope, Rope)] =
    if index == 0 then Some(Leaf(""), this)
    else if weight > index then {
      left.split(index).map { case (first, second) =>
        (first.rebalance, Node(second, right).rebalance)
      }
    } else if weight < index then {
      right.split(index - weight).map { case (first, second) =>
        (Node(left, first).rebalance, second.rebalance)
      }
    } else Some(left, right)

  override def index(i: Int): Option[Char] =
    if weight > i then left.index(i) else right.index(i - weight)

  private def rotateLeft(): Node = right match
    case Node(l, r)  => Node(Node(left, l), r)
    case Leaf(value) => this

  private def rotateRight(): Node = left match
    case Node(l, r)  => Node(l, Node(r, right))
    case Leaf(value) => this

  private def height(node: Rope): Int =
    node match
      case Node(l, r)  => Math.max(height(l), height(r)) + 1
      case Leaf(value) => 1

  private def subtreeWeight(rope: Rope): Int = rope match
    case Node(l, r)  => subtreeWeight(l) + subtreeWeight(r)
    case Leaf(value) => value.length
