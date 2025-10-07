package app.gui

import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.gui2.{Panel, GridLayout, Label, TextBox, AbstractComponent}
import java.util.concurrent.atomic.AtomicReference
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class EditorLayoutManager(editorTextBox: EditorTextBox) {
  val MIN_FILE_PANE_WIDTH = 20
  val MIN_EDITOR_WIDTH = 40
  private val MIN_HEIGHT = 10
  private val FILE_PANE_RATIO = 0.25

  private val lastKnownSize = new AtomicReference[TerminalSize](new TerminalSize(100, 30))

  private val filePane = new Panel()
  filePane.addComponent(new Label("File Pane"))

  private val panel = new Panel()
  private val gridLayout = new GridLayout(2)

  // Configure grid layout for stability
  gridLayout.setHorizontalSpacing(1)
  gridLayout.setVerticalSpacing(0)
  gridLayout.setLeftMarginSize(0)
  gridLayout.setRightMarginSize(0)
  gridLayout.setTopMarginSize(0)
  gridLayout.setBottomMarginSize(0)
  panel.setLayoutManager(gridLayout)

  // Initialize components with proper constraints
  initializeLayout()

  private def initializeLayout(): Unit = {
    val initialSize = lastKnownSize.get
    val (filePaneSize, editorSize) = calculateProposedSizes(initialSize)

    filePane.setPreferredSize(filePaneSize)
    editorTextBox.textBox.setPreferredSize(editorSize) // Fix: use textBox property

    // Configure file pane layout data
    filePane.setLayoutData(
      GridLayout.createLayoutData(
        GridLayout.Alignment.FILL,
        GridLayout.Alignment.FILL,
        false, // Don't grow horizontally
        true, // Grow vertically
        1, // Take one cell
        1 // Span one row
      )
    )

    // Configure editor layout data
    editorTextBox.textBox.setLayoutData(
      GridLayout.createLayoutData(
        GridLayout.Alignment.FILL,
        GridLayout.Alignment.FILL,
        true, // Grow horizontally
        true, // Grow vertically
        1, // Take one cell
        1 // Span one row
      )
    )

    panel.addComponent(filePane)
    panel.addComponent(editorTextBox.textBox) // Fix: use textBox property

    // Initialize viewport size
    editorTextBox.updateSize(editorSize).unsafeRunSync()
  }

  def calculateProposedSizes(windowSize: TerminalSize): (TerminalSize, TerminalSize) = {
    val totalWidth = math.max(MIN_FILE_PANE_WIDTH + MIN_EDITOR_WIDTH, windowSize.getColumns)
    val height = math.max(MIN_HEIGHT, windowSize.getRows)

    val filePaneWidth = math.max(MIN_FILE_PANE_WIDTH, (totalWidth * FILE_PANE_RATIO).toInt)
    val editorWidth = totalWidth - filePaneWidth - 1 // -1 for spacing

    (
      new TerminalSize(filePaneWidth, height),
      new TerminalSize(editorWidth, height)
    )
  }

  def getPanel: Panel = panel

  def setFilePaneVisible(visible: Boolean): Unit = {
    if (visible) {
      if (!panel.getChildren.contains(filePane)) {
        panel.addComponent(0, filePane)
        val (filePaneSize, editorSize) = calculateProposedSizes(lastKnownSize.get)
        filePane.setPreferredSize(filePaneSize)
        // Update both size and viewport
        editorTextBox.updateSize(editorSize).unsafeRunSync()
      }
    } else {
      if (panel.getChildren.contains(filePane)) {
        panel.removeComponent(filePane)
        // Adjust editor size to take full width and update viewport
        val fullSize = lastKnownSize.get
        editorTextBox.updateSize(new TerminalSize(fullSize.getColumns, fullSize.getRows)).unsafeRunSync()
      }
    }
    panel.invalidate()
  }

  def updateLayout(windowSize: TerminalSize): Unit = {
    if (windowSize.getColumns > 0 && windowSize.getRows > 0) {
      lastKnownSize.set(windowSize)
      val (filePaneSize, editorSize) = calculateProposedSizes(windowSize)

      if (panel.getChildren.contains(filePane)) {
        filePane.setPreferredSize(filePaneSize)
      }

      // Update editor size and trigger viewport update
      editorTextBox.updateSize(editorSize).unsafeRunSync()

      panel.invalidate()
    }
  }

  def getFilePane: Panel = filePane

  def getEditorTextBox: EditorTextBox = editorTextBox

  // Validate current layout
  def validateLayout(): Boolean = {
    val currentSize = panel.getSize
    val expected = lastKnownSize.get
    currentSize.getColumns <= expected.getColumns &&
      currentSize.getRows <= expected.getRows
  }
}
