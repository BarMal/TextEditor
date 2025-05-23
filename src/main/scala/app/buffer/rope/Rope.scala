package app.buffer.rope

import scala.annotation.tailrec

trait Rope(using balance: Balance) {
  def weight: Int
  def height: Int

  def isWeightBalanced: Boolean
  def isHeightBalanced: Boolean

  def ::(that: Rope): Rope = Node(this, that).rebalance

  def rebuild: Rope = Rope(this.collect())

  def rebalance: Rope

  def index(i: Int): Option[Char]

  def splitAt(index: Int): Option[(Rope, Rope)]

  def insert(index: Int, char: Char): Rope =
    splitAt(index) match
      case Some(pre, post) =>
        (post :: (Leaf(char.toString) :: pre)).rebalance
      case None => this

  def deleteLeft(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteRight(start, Math.abs(count))
    else if start - count < 0 then deleteLeft(start, start)
    else
      (for {
        startAndRest <- splitAt(Math.max(0, start - count))
        (keepStart, rest) = startAndRest
        restAndEnd <- rest.splitAt(start)
        (_, keepEnd) = restAndEnd
      } yield Node(keepStart, keepEnd)).getOrElse(this)

  def deleteRight(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteLeft(start, Math.abs(count))
    else if start + count > weight then deleteRight(start, weight - start)
    else
      (for {
        startAndRest <- splitAt(Math.max(0, start))
        (start, rest) = startAndRest
        restAndEnd <- rest.splitAt(count)
        (_, end) = restAndEnd
      } yield Node(start, end)).getOrElse(this)

  def dropLeft(n: Int): Rope = deleteRight(0, n)

  def dropRight(n: Int): Rope = deleteLeft(weight, n)

  def replace(index: Int, char: Char): Rope =
    splitAt(index) match
      case Some((l, r)) =>
        Node(l, r.dropLeft(1) :: Leaf(char.toString)).rebalance
      case None => this

  def collect(): String = {
    def _collect(curr: Rope, acc: List[String]): List[String] =
      curr match
        case Node(left, right) =>
          _collect(left, acc) ::: _collect(right, acc)
        case Leaf(value) => value :: acc

    _collect(this, List.empty[String]).mkString
  }

  def search(term: String): Option[Int] = {

    def searchLeaves(leaves: List[Leaf]): SearchState =
      val space: String = leaves.map(_.value).mkString
      println(s"""Searching for '$term' in '$space'""")
      if space.length < term.length then SearchState.Poll
      else
        val maybeIndex: Int = space.indexOf(term)
        if maybeIndex >= 0 then SearchState.Found(maybeIndex)
        else
          leaves match
            case _ :: tail
                if tail.map(_.value).mkString.length >= term.length =>
              SearchState.PollAndPrune
            case _ => SearchState.Poll

    @tailrec
    def _search(
        current: Rope,
        toVisit: List[Rope],
        searchSpace: List[Leaf],
        indexOffset: Int
    ): Option[Int] =
      current match
        case leaf: Leaf =>
          val space: List[Leaf] = searchSpace.appended(leaf)
          println(space.map(_.value).mkString)
          searchLeaves(space) match
            case SearchState.Found(index) => Some(indexOffset + index)
            case SearchState.Poll =>
              toVisit match
                case head :: rest =>
                  _search(head, rest, space, indexOffset)
                case Nil => None
            case SearchState.PollAndPrune =>
              toVisit match
                case head :: rest =>
                  _search(
                    head,
                    rest,
                    space.tail,
                    indexOffset + space.head.weight
                  )
                case Nil => None
        case Node(left, right) =>
          _search(left, right :: toVisit, searchSpace, indexOffset)

    _search(this, List.empty[Rope], List.empty[Leaf], 0)
  }
}

object Rope {

  def apply(in: String)(using balance: Balance): Rope =
    if in.length <= balance.leafChunkSize then Leaf(in)
    else {
      val (left, right) = in.splitAt(Math.floorDiv(in.length, 2))
      Node(Rope(left), Rope(right)).rebalance
    }

}
