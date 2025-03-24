package app

package object buffer {

  extension [U](opt: Option[U]) {
    def mapOr[T](f: U => T)(default: T): T = opt.map(f).getOrElse(default)
    def mapOr[T](pf: PartialFunction[U, T])(default: T): T =
      opt.map(pf).getOrElse(default)
  }

}
