package app.algebra.impl

import app.algebra.BufferAlgebra
import app.buffer.{BufferState, TogglingSet, Formatting}
import app.buffer.rope.{Rope, Balance}
import app.action.Effect
import app.buffer.WriteMode
import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import app.buffer.withContent

class RefBufferAlgebra[F[_]: Sync](stateRef: Ref[F, BufferState]) extends BufferAlgebra[F] {
  def getContent: F[String] =
    stateRef.get.map(_.buffer.collect())

  def setContent(content: String)(using balance: Balance): F[Unit] =
    stateRef.update(_.withContent(content))

  def getCurrentState: F[BufferState] =
    stateRef.get

  def updateState(f: BufferState => BufferState): F[Unit] =
    stateRef.update(f)

  def getCursorPosition: F[Int] =
    stateRef.get.map(_.cursorPosition)

  def setCursorPosition(pos: Int): F[Unit] =
    stateRef.update(state => state.copy(cursorPosition = pos))

  def getViewportSize: F[Int] =
    stateRef.get.map(_.viewportSize)

  def setViewportSize(size: Int): F[Unit] =
    stateRef.update(_.withViewportSize(size))

  def getSelection: F[TogglingSet[Int]] =
    stateRef.get.map(_.selected)

  def clearSelection: F[Unit] =
    stateRef.update(state => state.copy(selected = TogglingSet.empty))

  def getFormatting: F[Map[Int, Set[Formatting]]] =
    stateRef.get.map(_.formattingMap)

  def updateFormatting(f: Map[Int, Set[Formatting]] => Map[Int, Set[Formatting]]): F[Unit] =
    stateRef.update(state => state.copy(formattingMap = f(state.formattingMap)))

  def getWriteMode: F[WriteMode] =
    stateRef.get.map(_.writeMode)

  def setWriteMode(mode: WriteMode): F[Unit] =
    stateRef.update(state => state.copy(writeMode = mode))

  def recordEffect(effect: Effect): F[Unit] =
    stateRef.update(state => state.copy(userEffects = effect +: state.userEffects))
}
