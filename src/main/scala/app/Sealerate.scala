package app

object Sealerate {
  inline def valuesOf[T](using
      m: scala.deriving.Mirror.SumOf[T]
  ): Set[T] =
    allInstances[m.MirroredElemTypes, m.MirroredType].toSet

  private inline def allInstances[ET <: Tuple, T]: List[T] =
    import scala.compiletime.*

    inline erasedValue[ET] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[ValueOf[t]].value.asInstanceOf[T] :: allInstances[ts, T]
}
