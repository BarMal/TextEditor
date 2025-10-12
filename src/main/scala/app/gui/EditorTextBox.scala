package app.gui

import app.algebra.{BufferAlgebra, EditorUIAlgebra}
import app.buffer.rope.Balance
import cats.effect.kernel.Sync
import cats.syntax.all.*
import com.googlecode.lanterna.TerminalSize

/**
 * Editor component that synchronizes between UI and buffer state
 */
class EditorTextBox[F[_]: Sync](
    bufferAlg: BufferAlgebra[F],
    editorUI: EditorUIAlgebra[F]
) {
  private val MIN_VIEWPORT_SIZE = 100

  def initialize()(using Balance): F[Unit] = for {
    content <- bufferAlg.getContent
    _ <- editorUI.setText(content)
    size <- editorUI.getSize
    _ <- updateSize(size)
  } yield ()

  def updateSize(size: TerminalSize): F[Unit] = {
    val viewportSize = math.max(MIN_VIEWPORT_SIZE, size.getColumns * (size.getRows - 3)) // Account for header and margins
    for {
      _ <- editorUI.setSize(size)
      _ <- bufferAlg.setViewportSize(viewportSize)
    } yield ()
  }
}
