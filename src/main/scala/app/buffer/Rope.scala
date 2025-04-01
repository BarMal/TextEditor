package app.buffer

trait Rope(using balance: Balance) {
  def weight: Int

  def isWeightBalanced: Boolean
  def isHeightBalanced: Boolean

  def ::(that: Rope): Rope = Node(this, that).rebalance

  def rebuild: Rope = Rope(this.collect())

  def rebalance: Rope

  def index(i: Int): Option[Char]

  def split(index: Int): Option[(Rope, Rope)]

  def insert(index: Int, char: Char): Rope =
    split(index) match
      case Some(pre, post) => (post :: Leaf(char.toString) :: pre).rebalance
      case None            => this

  def delete(start: Int, count: Int): Rope =
    List(start, start + count).sorted match
      case _start :: _end :: _ =>
        (for {
          startSplit <- split(_start)
          endSplit   <- split(_end)
        } yield {
          val (keepStart, _) = startSplit
          val (_, keepEnd)   = endSplit
          Node(keepStart, keepEnd).rebalance
        }).getOrElse(this)
      case _ => this

  def drop(n: Int): Rope = delete(0, n)

  def replace(index: Int, char: Char): Rope =
    split(index) match
      case Some((l, r)) =>
        Node(l, r.drop(1)::Leaf(char.toString).rebalance)
      case None => this

  def collect(): String = {
    def _collect(curr: Rope, acc: List[String]): List[String] =
      curr match
        case Node(left, right) =>
          _collect(left, acc) ::: _collect(right, acc)
        case Leaf(value) => value :: acc

    _collect(this, List.empty[String]).mkString
  }
}

object Rope {

  def apply(in: String)(using balance: Balance): Rope =
    if in.length <= balance.leafChunkSize then Leaf(in)
    else {
      val (left, right) = in.splitAt(Math.floorDiv(in.length, 2))
      Node(Rope(left), Rope(right))
    }

}
