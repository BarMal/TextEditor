package app.buffer

trait Rope(using balance: Balance) {
  def weight: Int

  def isWeightBalanced: Boolean
  def isHeightBalanced: Boolean

  def ::(that: Rope): Rope = Node(Some(this), Some(that)).rebalance

  def rebuild: Rope = Rope(this.collect())

  def rebalance: Rope

  def index(i: Int): Option[Char]

  def split(index: Int): (Option[Rope], Option[Rope])

  def insert(index: Int, char: Char): Rope = {
    val (pre, post) = split(index)
    val leaf: Leaf  = Leaf(char.toString)
    (for {
      a <- pre
      b <- post
    } yield (b :: leaf :: a).rebalance).getOrElse(leaf)
  }

  def delete(start: Int, end: Int): Rope = {
    val (keepStart, _) = split(start)
    val (_, keepEnd)   = split(end)
    Node(keepStart, keepEnd).rebalance
  }

  def collect(): String = {
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

  def apply(in: String)(using balance: Balance): Rope =
    if in.length <= balance.leafChunkSize then Leaf(in)
    else {
      val (left, right) = in.splitAt(Math.floorDiv(in.length, 2))
      Node(Some(Rope(left)), Some(Rope(right)))
    }

  def empty(using balance: Balance): Rope = Node(None, None)

}
