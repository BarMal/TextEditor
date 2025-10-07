package app.algebra

import com.googlecode.lanterna.gui2.{BasicWindow, MultiWindowTextGUI}

/**
 * Algebra for GUI operations
 */
trait GUIAlgebra[F[_]] {
  def addWindow(window: BasicWindow): F[Unit]
  def updateScreen: F[Unit]
  def invalidateComponent(window: BasicWindow): F[Unit]
}
