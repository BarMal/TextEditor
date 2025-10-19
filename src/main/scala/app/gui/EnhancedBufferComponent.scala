package app.gui

import app.buffer.BufferState
import app.config.{CursorConfig, EditorConfig}
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.IORuntime
import com.googlecode.lanterna.{TerminalPosition, TerminalSize, TextColor}
import com.googlecode.lanterna.gui2.Interactable.Result
import com.googlecode.lanterna.gui2.{AbstractInteractableComponent, InteractableRenderer, TextGUIGraphics}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}

/** Enhanced BufferComponent with additional features:
  *   - Line numbers
  *   - Status bar info
  *   - Multiple cursor styles
  *   - Smooth scrolling
  */
class EnhancedBufferComponent(
    stateRef: Ref[IO, BufferState],
    config: EditorConfig = EditorConfig.default
)(using runtime: IORuntime)
    extends AbstractInteractableComponent[EnhancedBufferComponent] {

  private var cursorVisible = true
  private var scrollOffset  = 0 // Top line shown

  def onBlinkTick(): Unit = {
    cursorVisible = !cursorVisible
    invalidate()
  }

  override def handleKeyStroke(key: KeyStroke): Result = {
    // Handle special keys first
    key.getKeyType match {
      case KeyType.F1 =>
        // Toggle line numbers
        config.showLineNumbers = !config.showLineNumbers
        invalidate()
        return Result.HANDLED

//      case KeyType.F2 =>
//        // Cycle cursor styles
//        config.cursorStyle = config.cursorStyle match {
//          case CursorConfig.default     => CursorConfig.underscore
//          case CursorConfig.underscore  => CursorConfig.block
//          case CursorConfig.block       => CursorConfig.verticalBar
//          case CursorConfig.verticalBar => CursorConfig.default
//          case _                        => CursorConfig.default
//        }
//        invalidate()
//        return Result.HANDLED

      case _ => // Continue to normal handling
    }

    // Normal key handling through your effects
    stateRef.update(_ ++ key).unsafeRunSync()
    cursorVisible = true

    // Auto-scroll to keep cursor visible
    updateScrollOffset()

    invalidate()
    Result.HANDLED
  }

  private def updateScrollOffset(): Unit = {
    val state      = stateRef.get.unsafeRunSync()
    val content    = state.buffer.collect()
    val cursorLine = content.take(state.cursorPosition).count(_ == '\n')
    val height     = getSize.getRows

    // Keep cursor in middle third of screen
    val topThird    = height / 3
    val bottomThird = height - topThird

    if (cursorLine < scrollOffset + topThird) {
      scrollOffset = Math.max(0, cursorLine - topThird)
    } else if (cursorLine > scrollOffset + bottomThird) {
      scrollOffset = cursorLine - bottomThird
    }
  }

  override def createDefaultRenderer()
      : InteractableRenderer[EnhancedBufferComponent] =
    new InteractableRenderer[EnhancedBufferComponent] {

      override def getPreferredSize(
          component: EnhancedBufferComponent
      ): TerminalSize =
        component.getPreferredSize

      override def drawComponent(
          graphics: TextGUIGraphics,
          component: EnhancedBufferComponent
      ): Unit = {
        val g = graphics
        g.setBackgroundColor(config.cursorStyle.backgroundColor)
        g.setForegroundColor(config.cursorStyle.textColor)
        g.fill(' ')

        val state   = stateRef.get.unsafeRunSync()
        val content = state.buffer.collect()
        val lines   = content.split("\n", -1)

        val width  = g.getSize.getColumns
        val height = g.getSize.getRows

        // Calculate line number column width
        val lineNumWidth = if (config.showLineNumbers) {
          Math.max(3, lines.length.toString.length + 1)
        } else 0

        val textStartX = lineNumWidth

        // Calculate cursor position
        val textBeforeCursor = content.take(state.cursorPosition)
        val cursorLine       = textBeforeCursor.count(_ == '\n')
        val lastNewline      = textBeforeCursor.lastIndexOf('\n')
        val cursorColumn = if (lastNewline == -1) {
          state.cursorPosition
        } else {
          state.cursorPosition - lastNewline - 1
        }

        // Draw visible lines
        val visibleLines = lines.slice(scrollOffset, scrollOffset + height)

        visibleLines.zipWithIndex.foreach { case (line, idx) =>
          val y             = idx
          val actualLineNum = scrollOffset + idx + 1

          if (y < height) {
            // Draw line numbers
            if (config.showLineNumbers) {
              val lineNumStr = actualLineNum.toString.reverse
                .padTo(lineNumWidth - 1, ' ')
                .reverse
              g.setForegroundColor(TextColor.ANSI.BLUE)
              g.putString(0, y, lineNumStr + " ")
              g.setForegroundColor(config.cursorStyle.textColor)
            }

            // Draw line content
            val displayLine = line.take(width - textStartX)
            g.putString(textStartX, y, displayLine)

            // Draw cursor if on this line
            val isOnCursorLine = (scrollOffset + idx) == cursorLine
            if (isOnCursorLine && cursorVisible) {
              val cursorX =
                textStartX + Math.min(cursorColumn, displayLine.length)

              if (cursorX < width) {
                val charAtCursor = if (cursorColumn < line.length) {
                  line.charAt(cursorColumn)
                } else ' '

                val displayChar =
                  if (config.cursorStyle.showCharacterUnderCursor) {
                    charAtCursor
                  } else {
                    config.cursorStyle.cursorChar
                  }

                // Draw cursor with special formatting
                g.setBackgroundColor(config.cursorStyle.cursorBackgroundColor)
                g.setForegroundColor(config.cursorStyle.cursorForegroundColor)
                g.setCharacter(cursorX, y, displayChar)

                // Reset colors
                g.setBackgroundColor(config.cursorStyle.backgroundColor)
                g.setForegroundColor(config.cursorStyle.textColor)
              }
            }
          }
        }

        // Draw status line at bottom (if enabled)
        if (config.showStatusLine) {
          drawStatusLine(
            g,
            state,
            cursorLine + 1,
            cursorColumn + 1,
            width,
            height
          )
        }
      }

      private def drawStatusLine(
          g: TextGUIGraphics,
          state: BufferState,
          line: Int,
          col: Int,
          width: Int,
          height: Int
      ): Unit = {
        val statusY = height - 1

        // Status info
        val leftStatus   = s" ${state.writeMode} "
        val centerStatus = s"Ln $line, Col $col"
        val rightStatus  = s" ${state.buffer.weight} chars "

        // Calculate positions
        val centerX = (width - centerStatus.length) / 2
        val rightX  = width - rightStatus.length

        // Draw status bar
        g.setBackgroundColor(TextColor.ANSI.WHITE)
        g.setForegroundColor(TextColor.ANSI.BLACK)

        // Left
        g.putString(0, statusY, leftStatus)

        // Center
        if (
          centerX > leftStatus.length && centerX + centerStatus.length < rightX
        ) {
          g.putString(centerX, statusY, centerStatus)
        }

        // Right
        g.putString(rightX, statusY, rightStatus)

        // Fill gaps
        for (x <- leftStatus.length until centerX)
          g.setCharacter(x, statusY, ' ')
        for (x <- (centerX + centerStatus.length) until rightX)
          g.setCharacter(x, statusY, ' ')
      }

      override def getCursorLocation(
          component: EnhancedBufferComponent
      ): TerminalPosition = {
        val state            = stateRef.get.unsafeRunSync()
        val content          = state.buffer.collect()
        val textBeforeCursor = content.take(state.cursorPosition)
        val cursorLine       = textBeforeCursor.count(_ == '\n')
        val cursorColumn =
          state.cursorPosition - textBeforeCursor.lastIndexOf('\n') - 1

        val lineNumWidth = if (config.showLineNumbers) {
          val lines = content.split("\n", -1)
          Math.max(3, lines.length.toString.length + 1)
        } else 0

        val visibleY = cursorLine - scrollOffset
        val visibleX = lineNumWidth + cursorColumn

        new TerminalPosition(visibleX, visibleY)
      }
    }
}
