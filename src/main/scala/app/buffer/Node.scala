package app.buffer

case class Node(left: Rope, right: Rope)(using balance: Balance) extends Rope:

  override def weight: Int = subtreeWeight(left) + subtreeWeight(right)
  override def height: Int = Math.max(left.height, right.height) + 1

  override def isWeightBalanced: Boolean =
    Math.abs(left.weight - right.weight) <= balance.weightBalance

  override def isHeightBalanced: Boolean =
    Math.abs(left.height - right.height) <= balance.heightBalance

  override def rebalance: Rope = if isWeightBalanced then this
  else if left.weight < right.weight then rotateLeft()
  else rotateRight()

  override def split(index: Int): Option[(Rope, Rope)] =
    if index == 0 then Some(Leaf(""), this)
    else if weight == index then Some(this, Leaf(""))
    else if left.weight == index then Some(left, right)
    else if index < left.weight then
      left.split(index).map { case (first, second) =>
        (first, Node(second, right))
      }
    else
      right.split(index - left.weight).map { case (first, second) =>
        (Node(left, first), second)
      }

  override def index(i: Int): Option[Char] =
    if i < left.weight then left.index(i) else right.index(i - left.weight)

  private def rotateLeft(): Rope = right match
    case Node(l, r)  => Node(Node(left, l), r)
    case Leaf(value) => this

  private def rotateRight(): Rope = left match
    case Node(l, r)  => Node(l, Node(r, right))
    case Leaf(value) => this

  private def subtreeWeight(rope: Rope): Int = rope match
    case Node(l, r)  => subtreeWeight(l) + subtreeWeight(r)
    case Leaf(value) => value.length
