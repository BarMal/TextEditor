package app.gui

import com.googlecode.lanterna.TerminalSize

/**
 * Handles validation of layout changes before they are applied
 */
class LayoutValidator(layoutManager: EditorLayoutManager) {
  private val MIN_COLUMNS = 40
  private val MIN_ROWS = 10

  /**
   * Validates if a proposed new size would result in a valid layout
   */
  def validateNewSize(newSize: TerminalSize): Boolean = {
    // Basic size constraints
    if (newSize.getColumns < MIN_COLUMNS || newSize.getRows < MIN_ROWS) {
      return false
    }

    // Calculate the proposed sizes without actually updating the layout
    val (filePaneSize, editorSize) = layoutManager.calculateProposedSizes(newSize)
    
    // Verify minimum widths are maintained
    if (filePaneSize.getColumns < layoutManager.MIN_FILE_PANE_WIDTH) {
      return false
    }
    if (editorSize.getColumns < layoutManager.MIN_EDITOR_WIDTH) {
      return false
    }

    // Verify total width allocation is valid
    val totalWidth = filePaneSize.getColumns + editorSize.getColumns + 1 // +1 for spacing
    if (totalWidth > newSize.getColumns) {
      return false
    }

    true
  }
}
