package app.buffer.rope

import scala.annotation.tailrec
import scala.io.Source

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

  def insert(index: Int, str: String): Rope =
    splitAt(index) match
      case Some(pre, post) =>
        (post :: (Leaf(str) :: pre)).rebalance
      case None => this

  def delete(startIndex: Int, endIndex: Int): Rope =
    deleteRight(startIndex, endIndex - startIndex)

  def deleteLeft(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteRight(start, Math.abs(count))
    else if start - count <= 0 then deleteLeft(start, start)
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

  def replaceAll(term: String, replacement: String): Rope =
    searchAll(term).sorted.reverse.foldLeft(this)((rope, index) =>
      rope.delete(index, term.length).insert(index, replacement)
    )

  def collect(): String = {
    def _collect(curr: Rope, acc: Vector[String]): Vector[String] =
      curr match
        case Node(left, right) =>
          _collect(left, acc) appendedAll _collect(right, acc)
        case Leaf(value) => value +: acc

    _collect(this, Vector.empty[String]).mkString
  }

  def slice(startIndex: Int, endIndex: Int): Rope =
    dropRight(weight - endIndex).dropLeft(startIndex)

  def searchAll(term: String): List[Int] = {

    def searchLeaves(leaves: Vector[Leaf]): SearchState =
      val space: String = leaves.map(_.value).mkString
      if space.length < term.length then SearchState.Poll
      else
        val maybeIndex: Int = space.indexOf(term)
        if maybeIndex >= 0 then SearchState.Found(maybeIndex)
        else
          // Check if we need to keep the last few leaves for potential matches
          // that span across leaf boundaries
          val keepSize  = term.length - 1
          val spaceTail = space.takeRight(keepSize)
          if spaceTail.length >= term.length - 1 then SearchState.PollAndPrune
          else SearchState.Poll

    @tailrec
    def _searchAll(
        current: Rope,
        toVisit: Vector[Rope],
        searchSpace: Vector[Leaf],
        indexOffset: Int,
        foundResults: List[Int]
    ): List[Int] =
      current match
        case leaf: Leaf =>
          val space: Vector[Leaf] = searchSpace.appended(leaf)
          searchLeaves(space) match
            case SearchState.Found(index) =>
              val absoluteIndex = indexOffset + index
              // Continue searching in the same space for overlapping matches
              // by advancing just past the found match
              val searchedString        = space.map(_.value).mkString
              val remainingStartInSpace = index + 1

              // Calculate which leaves to keep based on where match was found
              val (leavesBefore, leavesFromMatch) = space.span { l =>
                space
                  .takeWhile(_ != l)
                  .map(_.value)
                  .mkString
                  .length < remainingStartInSpace
              }

              val newOffset = indexOffset + remainingStartInSpace

              toVisit.toList match
                case head :: rest =>
                  _searchAll(
                    head,
                    rest.toVector,
                    leavesFromMatch,
                    newOffset,
                    absoluteIndex :: foundResults
                  )
                case Nil =>
                  // No more nodes to visit, but check remaining space
                  if leavesFromMatch.nonEmpty &&
                    leavesFromMatch.map(_.value).mkString.contains(term)
                  then
                    val finalSpace = leavesFromMatch.map(_.value).mkString
                    val finalIndex = finalSpace.indexOf(term)
                    if finalIndex >= 0 then
                      (newOffset + finalIndex) :: absoluteIndex :: foundResults
                    else absoluteIndex :: foundResults
                  else absoluteIndex :: foundResults

            case SearchState.Poll =>
              toVisit.toList match
                case head :: rest =>
                  _searchAll(
                    head,
                    rest.toVector,
                    space,
                    indexOffset,
                    foundResults
                  )
                case Nil => foundResults

            case SearchState.PollAndPrune =>
              toVisit.toList match
                case head :: rest =>
                  // Keep last (term.length - 1) characters worth of leaves
                  val prunedSpace = pruneLeaves(space, term.length - 1)
                  val prunedOffset = indexOffset +
                    (space.map(_.weight).sum - prunedSpace.map(_.weight).sum)
                  _searchAll(
                    head,
                    rest.toVector,
                    prunedSpace,
                    prunedOffset,
                    foundResults
                  )
                case Nil => foundResults

        case Node(left, right) =>
          _searchAll(
            left,
            right +: toVisit,
            searchSpace,
            indexOffset,
            foundResults
          )

    _searchAll(
      this,
      Vector.empty[Rope],
      Vector.empty[Leaf],
      0,
      List.empty[Int]
    ).reverse.distinct
  }

  private def pruneLeaves(
      leaves: Vector[Leaf],
      keepChars: Int
  ): Vector[Leaf] = {
    @tailrec
    def go(remaining: Vector[Leaf], accumulated: Int): Vector[Leaf] =
      if accumulated >= keepChars || remaining.isEmpty then remaining
      else go(remaining.tail, accumulated + remaining.head.weight)

    go(leaves.reverse, 0).reverse
  }

  def search(term: String): Option[Int] = {

    def searchLeaves(leaves: Vector[Leaf]): SearchState =
      val space: String = leaves.map(_.value).mkString
      if space.length < term.length then SearchState.Poll
      else
        val maybeIndex: Int = space.indexOf(term)
        if maybeIndex >= 0 then SearchState.Found(maybeIndex)
        else
          // Keep enough leaves to catch matches spanning boundaries
          val keepSize = term.length - 1
          if leaves.size > 1 && leaves
              .map(_.value)
              .mkString
              .takeRight(keepSize)
              .length >= keepSize
          then SearchState.PollAndPrune
          else SearchState.Poll

    @tailrec
    def _search(
        current: Rope,
        toVisit: Vector[Rope],
        searchSpace: Vector[Leaf],
        indexOffset: Int
    ): Option[Int] =
      current match
        case leaf: Leaf =>
          val space: Vector[Leaf] = searchSpace.appended(leaf)
          searchLeaves(space) match
            case SearchState.Found(index) => Some(indexOffset + index)
            case SearchState.Poll =>
              toVisit.toList match
                case head :: rest =>
                  _search(head, rest.toVector, space, indexOffset)
                case Nil => None
            case SearchState.PollAndPrune =>
              toVisit.toList match
                case head :: rest =>
                  val prunedSpace = pruneLeaves(space, term.length - 1)
                  val prunedOffset = indexOffset +
                    (space.map(_.weight).sum - prunedSpace.map(_.weight).sum)
                  _search(
                    head,
                    rest.toVector,
                    prunedSpace,
                    prunedOffset
                  )
                case Nil => None
        case Node(left, right) =>
          _search(left, right +: toVisit, searchSpace, indexOffset)

    _search(this, Vector.empty[Rope], Vector.empty[Leaf], 0)
  }
}

object Rope {

  def empty(using balance: Balance): Rope = Leaf("")

  def apply(in: String)(using balance: Balance): Rope =
    if in.length <= balance.leafChunkSize then Leaf(in)
    else {
      val (left, right) = in.splitAt(Math.floorDiv(in.length, 2))
      Node(Rope(left), Rope(right)).rebalance
    }

  def mobyDick(using balance: Balance): Rope =
    Rope(Source.fromResource("MobyDick.txt").mkString)
}
