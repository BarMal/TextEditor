package app.algebra.impl

import app.algebra.GUIAlgebra
import com.googlecode.lanterna.gui2.{BasicWindow, MultiWindowTextGUI, Panel}
import cats.effect.Sync
import cats.syntax.functor.*

class LanternaGUIAlgebra[F[_]: Sync](textGUI: MultiWindowTextGUI) extends GUIAlgebra[F] {
  def addWindow(window: BasicWindow): F[Unit] =
    Sync[F].delay(textGUI.addWindowAndWait(window))

  def updateScreen: F[Unit] =
    Sync[F].delay(textGUI.updateScreen())

  def invalidateComponent(window: BasicWindow): F[Unit] =
    Sync[F].delay(window.getComponent.asInstanceOf[Panel].invalidate())
}
