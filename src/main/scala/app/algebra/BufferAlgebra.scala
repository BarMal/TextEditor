package app.algebra

import app.buffer.{BufferState, TogglingSet, Formatting}
import app.buffer.rope.{Rope, Balance}
import app.action.Effect
import app.buffer.WriteMode
import cats.Monad

/**
 * Algebra for buffer operations
 */
trait BufferAlgebra[F[_]] {
  def getContent: F[String]
  def setContent(content: String)(using Balance): F[Unit]
  def getCurrentState: F[BufferState]
  def updateState(f: BufferState => BufferState): F[Unit]
  def getCursorPosition: F[Int]
  def setCursorPosition(pos: Int): F[Unit]
  def getViewportSize: F[Int]
  def setViewportSize(size: Int): F[Unit]
  def getSelection: F[TogglingSet[Int]]
  def clearSelection: F[Unit]
  def getFormatting: F[Map[Int, Set[Formatting]]]
  def updateFormatting(f: Map[Int, Set[Formatting]] => Map[Int, Set[Formatting]]): F[Unit]
  def getWriteMode: F[WriteMode]
  def setWriteMode(mode: WriteMode): F[Unit]
  def recordEffect(effect: Effect): F[Unit]
}
