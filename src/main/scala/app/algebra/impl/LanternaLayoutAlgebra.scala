package app.algebra.impl

import app.algebra.{LayoutAlgebra, LayoutState}
import app.gui.EditorTextBox
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.implicits.toFlatMapOps
import cats.syntax.functor.*
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.gui2.{GridLayout, Panel}
import com.googlecode.lanterna.input.KeyStroke

import java.util.concurrent.atomic.AtomicReference

class LanternaLayoutAlgebra[F[_]: Sync](
    panel: Panel,
    filePane: Panel,
    editorBox: EditorTextBox[F],
    lastKnownSize: AtomicReference[TerminalSize]
) extends LayoutAlgebra[F] {
  val MIN_FILE_PANE_WIDTH     = 20
  val MIN_EDITOR_WIDTH        = 40
  private val MIN_HEIGHT      = 10
  private val FILE_PANE_RATIO = 0.25

  private val layoutStateRef: Ref[F, LayoutState] = Ref.unsafe[F, LayoutState](
    LayoutState(
      currentSize = Some(lastKnownSize.get),
      position = None,
      isFilePaneVisible = true
    )
  )

  def updateLayout(windowSize: TerminalSize): F[Unit] =
    for {
      _     <- Sync[F].delay(lastKnownSize.set(windowSize))
      sizes <- calculateProposedSizes(windowSize)
      (filePaneSize, editorSize) = sizes
      _ <- Sync[F].delay {
        if (panel.getChildren.contains(filePane)) {
          filePane.setPreferredSize(filePaneSize)
        }
      }
      _ <- editorBox.updateSize(editorSize)
      _ <- refreshLayout
    } yield ()

  def validateLayout(size: TerminalSize): F[Boolean] =
    Sync[F].delay {
      if (
        size.getColumns < MIN_FILE_PANE_WIDTH + MIN_EDITOR_WIDTH || size.getRows < MIN_HEIGHT
      ) {
        false
      } else {
        val currentSize = panel.getSize
        val expected    = lastKnownSize.get
        currentSize.getColumns <= expected.getColumns &&
        currentSize.getRows <= expected.getRows
      }
    }

  def handleInput(input: KeyStroke): F[Unit] =
    Sync[F].delay {
      // Handle layout-specific input commands
      input.getCharacter match {
        case 'f' | 'F' => setFilePaneVisible(false)
        case 'v' | 'V' => setFilePaneVisible(true)
        case _         => Sync[F].unit
      }
    }.flatten

  def handleUnhandledInput(input: KeyStroke): F[Unit] =
    Sync[F].delay(println(s"Unhandled layout input: ${input.getCharacter}"))

  def handleWindowMove(position: (Int, Int)): F[Unit] =
    layoutStateRef.update(state => state.copy(position = Some(position)))

  def getLastKnownState: F[LayoutState] = layoutStateRef.get

  def calculateProposedSizes(
      windowSize: TerminalSize
  ): F[(TerminalSize, TerminalSize)] =
    for {
      state <- layoutStateRef.get
      sizes <- Sync[F].delay {
        val totalWidth = windowSize.getColumns
        val height     = windowSize.getRows

        val filePaneWidth = if (state.isFilePaneVisible) {
          math.max(MIN_FILE_PANE_WIDTH, (totalWidth * FILE_PANE_RATIO).toInt)
        } else 0

        val editorWidth = totalWidth - filePaneWidth

        (
          new TerminalSize(filePaneWidth, height),
          new TerminalSize(editorWidth, height)
        )
      }
    } yield sizes

  def setFilePaneVisible(visible: Boolean): F[Unit] =
    for {
      _ <- Sync[F].delay {
        if (visible && !panel.getChildren.contains(filePane)) {
          panel.addComponent(
            0,
            filePane
          ) // Fixed argument order for addComponent
        } else if (!visible && panel.getChildren.contains(filePane)) {
          panel.removeComponent(filePane)
        }
      }
      _ <- layoutStateRef.update(_.copy(isFilePaneVisible = visible))
      size = lastKnownSize.get
      _ <- updateLayout(size)
    } yield ()

  def refreshLayout: F[Unit] =
    Sync[F].delay(panel.invalidate())
}
