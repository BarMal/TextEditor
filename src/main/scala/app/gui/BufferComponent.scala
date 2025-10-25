package app.gui

import app.buffer.BufferState
import app.buffer.rope.Balance
import app.config.{CursorConfig, EditorConfig}
import cats.effect.kernel.Ref
import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.gui2.Interactable.Result
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.{TerminalPosition, TerminalSize, TextColor}

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable

/** A Lanterna component that displays and edits a Rope-based buffer with a
  * customizable blinking cursor
  */
class BufferComponent(
    stateRef: Ref[IO, BufferState],
    cursorConfig: CursorConfig = CursorConfig.default
)(using runtime: IORuntime)
    extends AbstractInteractableComponent[BufferComponent] {

  /** Handles keystrokes and updates BufferState using your existing effects */
  override def handleKeyStroke(key: KeyStroke): Result = {
    stateRef.update(_ ++ key).unsafeRunSync()
    invalidate()
    Result.HANDLED
  }

  /** Custom renderer that uses BufferState and displays cursor */
  override def createDefaultRenderer(): InteractableRenderer[BufferComponent] =
    new InteractableRenderer[BufferComponent] {

      override def drawComponent(
          graphics: TextGUIGraphics,
          component: BufferComponent
      ): Unit = {
        graphics.setBackgroundColor(cursorConfig.backgroundColor)
        graphics.setForegroundColor(cursorConfig.textColor)
        graphics.fill(' ')

        // Get current buffer state and dimensions
        val state  = stateRef.get.unsafeRunSync()
        val width  = graphics.getSize.getColumns
        val height = graphics.getSize.getRows

        // Build visible lines with proper word wrapping
        val visibleLines = buildVisibleLines(state, width, height)

        // Calculate cursor position in screen coordinates
        val (cursorScreenCol, cursorScreenRow) =
          calculateCursorPosition(state, visibleLines, width)

        // Draw visible lines
        visibleLines.zipWithIndex.foreach { case (line, y) =>
          if (y < height) {
            graphics.putString(0, y, line.take(width))
          }
        }

        // Draw cursor
        if (
          cursorScreenRow >= 0 && cursorScreenRow < height &&
          cursorScreenCol >= 0 && cursorScreenCol < width
        ) {
          val charAtCursor = if (cursorScreenRow < visibleLines.length) {
            val line = visibleLines(cursorScreenRow)
            if (cursorScreenCol < line.length) line.charAt(cursorScreenCol)
            else ' '
          } else ' '

          graphics.setBackgroundColor(cursorConfig.cursorBackgroundColor)
          graphics.setForegroundColor(cursorConfig.cursorForegroundColor)
          graphics.setCharacter(
            cursorScreenCol,
            cursorScreenRow,
            if (cursorConfig.showCharacterUnderCursor) charAtCursor
            else cursorConfig.cursorChar
          )
        }
      }

      private def buildVisibleLines(
          state: BufferState,
          width: Int,
          height: Int
      ): Vector[String] = {
        val content = state.buffer.collect()

        // Find the line containing the cursor
        val linesBeforeCursor =
          content.take(state.cursorPosition).count(_ == '\n')

        // Calculate how many screen lines we need before and after cursor
        val screenLinesNeeded = height
        val centerLine        = linesBeforeCursor

        // Get a range of buffer lines around the cursor
        val startBufferLine = Math.max(0, centerLine - height)
        val endBufferLine   = centerLine + height

        // Split content into buffer lines (by newlines)
        val allBufferLines = content.split("\n", -1)
        val bufferLinesToShow = allBufferLines.slice(
          startBufferLine,
          Math.min(endBufferLine, allBufferLines.length)
        )

        // Wrap each buffer line to screen width, preserving empty lines
        val wrappedLines = bufferLinesToShow.flatMap { line =>
          if (line.isEmpty) Vector("")
          else wrapLine(line, width)
        }.toVector

        // Find which wrapped line contains the cursor
        val cursorWrappedLine = findCursorWrappedLine(
          allBufferLines,
          startBufferLine,
          state.cursorPosition,
          width
        )

        // Center the view around the cursor's wrapped line
        val startIdx = Math.max(0, cursorWrappedLine - height / 2)
        val endIdx   = Math.min(wrappedLines.length, startIdx + height)

        wrappedLines.slice(startIdx, endIdx)
      }

      private def wrapLine(line: String, width: Int): Vector[String] =
        if (line.length <= width) Vector(line)
        else {
          val chunks = line.grouped(width).toVector
          chunks
        }

      private def findCursorWrappedLine(
          allLines: Array[String],
          startLine: Int,
          cursorPos: Int,
          width: Int
      ): Int = {
        var currentPos       = 0
        var wrappedLineCount = 0

        // Calculate position through all lines up to cursor
        for (i <- 0 until allLines.length) {
          val line       = allLines(i)
          val lineLength = line.length + 1 // +1 for newline

          if (currentPos + lineLength > cursorPos) {
            // Cursor is in this line
            val posInLine = cursorPos - currentPos
            val wrappedLinesInThisLine =
              Math.max(1, (line.length + width - 1) / width)
            val cursorWrappedOffset = posInLine / width
            return wrappedLineCount + cursorWrappedOffset
          }

          currentPos += lineLength
          wrappedLineCount += Math.max(1, (line.length + width - 1) / width)
        }

        wrappedLineCount
      }

      private def calculateCursorPosition(
          state: BufferState,
          visibleLines: Vector[String],
          width: Int
      ): (Int, Int) = {
        val content          = state.buffer.collect()
        val textBeforeCursor = content.take(state.cursorPosition)
        val cursorBufferLine = textBeforeCursor.count(_ == '\n')
        val lastNewline      = textBeforeCursor.lastIndexOf('\n')
        val posInBufferLine = if (lastNewline == -1) {
          state.cursorPosition
        } else {
          state.cursorPosition - lastNewline - 1
        }

        // Calculate wrapped position
        val wrappedRow = posInBufferLine / width
        val wrappedCol = posInBufferLine % width

        // Find the screen row (need to account for all previous wrapped lines)
        val allLines          = content.split("\n", -1)
        var screenRow         = 0
        var currentBufferLine = 0

        for (i <- 0 until Math.min(cursorBufferLine, allLines.length)) {
          val line = allLines(i)
          screenRow += Math.max(1, (line.length + width - 1) / width)
          currentBufferLine += 1
        }

        screenRow += wrappedRow

        // Adjust for viewport scrolling
        val centerLine    = screenRow
        val viewportStart = Math.max(0, centerLine - visibleLines.length / 2)
        val adjustedRow   = screenRow - viewportStart

        (wrappedCol, adjustedRow)
      }

      override def getCursorLocation(
          component: BufferComponent
      ): TerminalPosition = {
        val state        = stateRef.get.unsafeRunSync()
        val width        = component.getSize.getColumns
        val height       = component.getSize.getRows
        val visibleLines = buildVisibleLines(state, width, height)
        val (col, row)   = calculateCursorPosition(state, visibleLines, width)
        new TerminalPosition(col, row)
      }

      override def getPreferredSize(component: BufferComponent): TerminalSize =
        new TerminalSize(100, 80)
    }
}
