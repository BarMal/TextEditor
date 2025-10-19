package app.config

import com.googlecode.lanterna.TextColor

/** Configuration for cursor appearance and behavior
  */
case class CursorConfig(
    cursorChar: Char,
    cursorForegroundColor: TextColor,
    cursorBackgroundColor: TextColor,
    textColor: TextColor,
    backgroundColor: TextColor,
    showCharacterUnderCursor: Boolean
)

object CursorConfig {

  /** Default: white underscore on black background */
  def default: CursorConfig = CursorConfig(
    cursorChar = '_',
    cursorForegroundColor = TextColor.ANSI.WHITE,
    cursorBackgroundColor = TextColor.ANSI.BLACK,
    textColor = TextColor.ANSI.WHITE,
    backgroundColor = TextColor.ANSI.BLACK,
    showCharacterUnderCursor = false
  )

  /** Block cursor: shows character with inverted colors */
  def block: CursorConfig = CursorConfig(
    cursorChar = '█',
    cursorForegroundColor = TextColor.ANSI.BLACK,
    cursorBackgroundColor = TextColor.ANSI.WHITE,
    textColor = TextColor.ANSI.WHITE,
    backgroundColor = TextColor.ANSI.BLACK,
    showCharacterUnderCursor = true
  )

  /** Underscore cursor: traditional terminal style */
  def underscore: CursorConfig = CursorConfig(
    cursorChar = '_',
    cursorForegroundColor = TextColor.ANSI.WHITE_BRIGHT,
    cursorBackgroundColor = TextColor.ANSI.BLACK,
    textColor = TextColor.ANSI.WHITE,
    backgroundColor = TextColor.ANSI.BLACK,
    showCharacterUnderCursor = false
  )

  /** Vertical bar cursor: like VSCode */
  def verticalBar: CursorConfig = CursorConfig(
    cursorChar = '│',
    cursorForegroundColor = TextColor.ANSI.CYAN_BRIGHT,
    cursorBackgroundColor = TextColor.ANSI.BLACK,
    textColor = TextColor.ANSI.WHITE,
    backgroundColor = TextColor.ANSI.BLACK,
    showCharacterUnderCursor = false
  )
}
