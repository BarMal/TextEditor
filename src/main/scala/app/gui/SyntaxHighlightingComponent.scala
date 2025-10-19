package app.gui

import app.buffer.BufferState
import app.config.EditorConfig
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.IORuntime
import com.googlecode.lanterna.gui2.Interactable.Result
import com.googlecode.lanterna.input.KeyStroke

/** Bonus: Syntax highlighting component (Simplified example - integrate with
  * your formattingMap)
  */
class SyntaxHighlightingComponent(
    stateRef: Ref[IO, BufferState],
    config: EditorConfig,
    highlighter: String => Map[Int, Set[app.buffer.Formatting]]
)(using runtime: IORuntime)
    extends EnhancedBufferComponent(stateRef, config) {

  override def handleKeyStroke(key: KeyStroke): Result = {
    val result = super.handleKeyStroke(key)

    // Update syntax highlighting after each keystroke
    val state      = stateRef.get.unsafeRunSync()
    val content    = state.buffer.collect()
    val formatting = highlighter(content)

    stateRef.update(_.copy(formattingMap = formatting)).unsafeRunSync()

    result
  }
}

/** Simple Scala syntax highlighter example
  */
object SimpleHighlighter {
  import app.buffer.Formatting

  val keywords = Set(
    "def",
    "val",
    "var",
    "class",
    "object",
    "trait",
    "case",
    "if",
    "else",
    "for",
    "while",
    "match",
    "import",
    "package"
  )

  def highlight(content: String): Map[Int, Set[Formatting]] = {
    val formatting = scala.collection.mutable.Map[Int, Set[Formatting]]()

    // Very simple: highlight keywords
    keywords.foreach { keyword =>
      var index = 0
      while (index >= 0) {
        index = content.indexOf(keyword, index)
        if (index >= 0) {
          // Check if it's a whole word (not part of identifier)
          val isWholeWord =
            (index == 0 || !content.charAt(index - 1).isLetterOrDigit) &&
              (index + keyword.length >= content.length || !content
                .charAt(index + keyword.length)
                .isLetterOrDigit)

          if (isWholeWord) {
            (index until index + keyword.length).foreach { i =>
              formatting(i) = Set(Formatting.Bold)
            }
          }
          index += keyword.length
        }
      }
    }

    formatting.toMap
  }
}
