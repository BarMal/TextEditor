package app.buffer

case class TogglingSet[T] private (private val set: Set[T] = Set.empty[T])(using
    ordering: Ordering[T]
) {

  def offer(value: T): TogglingSet[T] =
    if set.contains(value) then TogglingSet(set.excl(value))
    else TogglingSet(set.incl(value))

  def exists(value: T): Boolean = set.contains(value)
  
  def isEmpty: Boolean = set.isEmpty

  def range: (T, T) = (set.min, set.max)

  override def toString: String = if set.isEmpty then "Nothing selected"
  else s"""${set.min}-${set.max}"""

}

object TogglingSet {

  def empty[T](using ordering: Ordering[T]): TogglingSet[T] =
    TogglingSet[T](Set.empty[T])

}
