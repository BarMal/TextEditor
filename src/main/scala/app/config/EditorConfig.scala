package app.config

import app.config.CursorConfig

/** Configuration for the enhanced editor
  */
case class EditorConfig(
    var cursorStyle: CursorConfig,
    var showLineNumbers: Boolean,
    var showStatusLine: Boolean,
    var tabSize: Int
)

object EditorConfig {
  def default: EditorConfig = EditorConfig(
    cursorStyle = CursorConfig.underscore,
    showLineNumbers = true,
    showStatusLine = true,
    tabSize = 4
  )

  def minimal: EditorConfig = EditorConfig(
    cursorStyle = CursorConfig.underscore,
    showLineNumbers = false,
    showStatusLine = false,
    tabSize = 4
  )

  def vimLike: EditorConfig = EditorConfig(
    cursorStyle = CursorConfig.block,
    showLineNumbers = true,
    showStatusLine = true,
    tabSize = 4
  )
}
