package app.algebra.impl

import app.algebra.EditorUIAlgebra
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.gui2.TextBox
import cats.effect.Sync
import cats.syntax.functor.*

class LanternaEditorUIAlgebra[F[_]: Sync](textBox: TextBox) extends EditorUIAlgebra[F] {
  def setText(content: String): F[Unit] =
    Sync[F].delay(textBox.setText(content))

  def setSize(size: TerminalSize): F[Unit] =
    Sync[F].delay(textBox.setPreferredSize(size))

  def getSize: F[TerminalSize] =
    Sync[F].delay(textBox.getSize)

}
