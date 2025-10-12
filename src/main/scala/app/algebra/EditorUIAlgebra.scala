package app.algebra

import com.googlecode.lanterna.TerminalSize
import cats.Monad

/**
 * Algebra for editor UI operations
 */
trait EditorUIAlgebra[F[_]] {
  def setText(content: String): F[Unit]
  def setSize(size: TerminalSize): F[Unit]
  def getSize: F[TerminalSize]
}
