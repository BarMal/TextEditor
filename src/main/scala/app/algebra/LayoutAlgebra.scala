package app.algebra

import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.input.KeyStroke

/**
 * Enhanced algebra for layout operations with input handling
 */
trait LayoutAlgebra[F[_]] {
  def updateLayout(size: TerminalSize): F[Unit]
  def validateLayout(size: TerminalSize): F[Boolean]
  def calculateProposedSizes(windowSize: TerminalSize): F[(TerminalSize, TerminalSize)]
  def setFilePaneVisible(visible: Boolean): F[Unit]
  def refreshLayout: F[Unit]
  
  // New methods for enhanced event handling
  def handleInput(input: KeyStroke): F[Unit]
  def handleUnhandledInput(input: KeyStroke): F[Unit]
  def handleWindowMove(position: (Int, Int)): F[Unit]
  def getLastKnownState: F[LayoutState]
}

/**
 * Represents the current state of the layout
 */
case class LayoutState(
  currentSize: Option[TerminalSize],
  position: Option[(Int, Int)],
  isFilePaneVisible: Boolean
)
