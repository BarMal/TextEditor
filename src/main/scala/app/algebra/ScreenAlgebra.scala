package app.algebra

import com.googlecode.lanterna.input.KeyStroke
import fs2.Stream

/**
 * Algebra for screen operations
 */
trait ScreenAlgebra[F[_]] {
  def readInput: Stream[F, KeyStroke]
  def startScreen: F[Unit]
  def stopScreen: F[Unit]
  def updateScreen: F[Unit]
}
