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

        // Get current buffer state
        val state = stateRef.get.unsafeRunSync()

        val width  = graphics.getSize.getColumns
        val height = 999999

        val boundedLineIndices: mutable.SortedSet[Int] =
          state.newLineIndices.union(Set(0, state.buffer.weight))

        val startLineIndex = boundedLineIndices
          .maxBefore(state.cursorPosition - (height / 2))
          .getOrElse(0)
        val endLineIndex = boundedLineIndices
          .minAfter(state.cursorPosition + (height / 2))
          .getOrElse(state.buffer.weight)

//        println(s"""SLI: $startLineIndex | ELI: $endLineIndex""")
//        val visibleLineIndices = state.newLineIndices.dropWhile(_ != start).takeWhile(_ != end)

        val visibleLines = boundedLineIndices
          .dropWhile(_ < startLineIndex)
          .takeWhile(_ <= endLineIndex)
        println(
          s"""SLI $startLineIndex | ELI $endLineIndex | CP ${state.cursorPosition} | W $width | H $height | VL ${visibleLines
              .mkString(", ")} | BL ${boundedLineIndices.mkString(", ")}"""
        )
        val lines: List[String] =
          visibleLines.toList
            .sliding(2)
            .collect { case start :: end :: Nil =>
              state.buffer
                .slice(start, end)
                .replaceAll("\n", "")
                .collect()
            }
            .toList

//        lines.foreach(line => println(s"""$line | ${line.count(_ == '\n')}"""))

        // Calculate which line the cursor is on
        val cursorLine =
          boundedLineIndices.count(_ < state.cursorPosition)
        val cursorColumn =
          state.cursorPosition - boundedLineIndices
            .maxBefore(state.cursorPosition)
            .getOrElse(0)

        // Draw visible lines
//        val startLine    = Math.max(0, cursorLine - height / 2)
//        val visibleLines = lines.slice(startLine, startLine + height)

        lines.zipWithIndex.foreach { case (line, y) =>
          // Draw line content
//            val displayLine = line.take(width)
          graphics.putString(0, y, line)
//          println(s"""line $line | index $y""")
        // Draw cursor if on this line
//            if (startLineIndex + y == cursorLine && cursorVisible.get()) {
//              val cursorX = Math.min(cursorColumn, width - 1)
//
//              // Get character at cursor position (or space if at end of line)
//              val charAtCursor = if (cursorColumn < line.length) {
//                line.charAt(cursorColumn)
//              } else {
//                ' '
//              }
//
//              // Draw cursor
//              val cursorChar = if (cursorConfig.showCharacterUnderCursor) {
//                charAtCursor
//              } else {
//                cursorConfig.cursorChar
//              }
//
//              graphics.setBackgroundColor(cursorConfig.cursorBackgroundColor)
//              graphics.setForegroundColor(cursorConfig.cursorForegroundColor)
//              graphics.setCharacter(cursorX, y, cursorChar)
//
//              // Reset colors for rest of drawing
//              graphics.setBackgroundColor(cursorConfig.backgroundColor)
//              graphics.setForegroundColor(cursorConfig.textColor)
//            }
        }
      }

      override def getCursorLocation(
          component: BufferComponent
      ): TerminalPosition = {
        val state            = stateRef.get.unsafeRunSync()
        val content          = state.buffer.collect()
        val textBeforeCursor = content.take(state.cursorPosition)
        val cursorLine       = textBeforeCursor.count(_ == '\n')
        val cursorColumn =
          state.cursorPosition - textBeforeCursor.lastIndexOf('\n') - 1

        // Calculate visible cursor position (accounting for scrolling)
        val height    = component.getSize.getRows
        val startLine = Math.max(0, cursorLine - height / 2)
        val visibleY  = cursorLine - startLine

        new TerminalPosition(cursorColumn, visibleY)
      }

      override def getPreferredSize(component: BufferComponent): TerminalSize =
        new TerminalSize(100, 80)
    }
}

/** Example: Main with enhanced component
  */
//object EnhancedMain {
//  def createEnhancedUI(
//      gui: MultiWindowTextGUI,
//      bufferState: Ref[IO, BufferState],
//      config: EditorConfig
//  )(using IORuntime): IO[(BasicWindow, EnhancedBufferComponent)] = IO.delay {
//    import scala.jdk.CollectionConverters.*
//
//    val window = new BasicWindow("BAM Editor - Enhanced")
//    window.setHints(
//      Set(
//        Window.Hint.FULL_SCREEN,
//        Window.Hint.NO_DECORATIONS
//      ).asJava
//    )
//
//    val mainPanel = new Panel()
//    val layout    = new GridLayout(2)
//    layout.setHorizontalSpacing(1)
//    mainPanel.setLayoutManager(layout)
//
//    // File pane
//    val filePanel = new Panel()
//    filePanel.setLayoutManager(new LinearLayout(Direction.VERTICAL))
//    filePanel.addComponent(new Label("Files"))
//
//    val fileList = new ActionListBox(new TerminalSize(25, 30))
//    fileList.addItem("file1.txt", () => ())
//    fileList.addItem("file2.scala", () => ())
//    fileList.addItem("file3.md", () => ())
//    filePanel.addComponent(fileList)
//
//    // Enhanced editor component
//    val bufferComponent = new EnhancedBufferComponent(bufferState, config)
//    bufferComponent.setPreferredSize(new TerminalSize(90, 35))
//
//    // Help bar at top
//    val helpPanel = new Panel()
//    helpPanel.setLayoutManager(new LinearLayout(Direction.HORIZONTAL))
//    helpPanel.addComponent(
//      new Label(" F1: Line Numbers | F2: Cursor Style | Ctrl+C: Exit ")
//    )
//
//    mainPanel.addComponent(filePanel)
//    mainPanel.addComponent(bufferComponent)
//
//    val rootPanel = new Panel()
//    rootPanel.setLayoutManager(new LinearLayout(Direction.VERTICAL))
//    rootPanel.addComponent(helpPanel)
//    rootPanel.addComponent(mainPanel)
//
//    window.setComponent(rootPanel)
//    (window, bufferComponent)
//  }
//
//  def runEnhanced(args: List[String])(using IORuntime): IO[ExitCode] = {
//    // Configuration
//    val cursorInterval = 500.milliseconds
//    val editorConfig   = EditorConfig.vimLike
//
//    (for {
//      config <- IO.fromEither(
//        pureconfig.ConfigSource.defaultApplication
//          .load[app.config.AppConfig]
//          .leftMap(failures => new RuntimeException(failures.toString))
//      )
//
//      exitCode <- createScreen(config).use { screen =>
//        for {
//          bufferState <- Ref[IO].of(app.buffer.BufferState.empty)
//          gui = new MultiWindowTextGUI(screen)
//
//          (window, bufferComponent) <- createEnhancedUI(
//            gui,
//            bufferState,
//            editorConfig
//          )
//
//          // Start blink stream
//          blinkFiber <- fs2.Stream
//            .fixedRate[IO](cursorInterval)
//            .evalTap(_ => IO.delay(bufferComponent.onBlinkTick()))
//            .compile
//            .drain
//            .start
//
//          _ <- IO.delay(gui.addWindowAndWait(window))
//          _ <- blinkFiber.cancel
//
//        } yield ExitCode.Success
//      }
//    } yield exitCode).handleErrorWith { error =>
//      IO.println(s"Error: ${error.getMessage}").as(ExitCode.Error)
//    }
//  }
//
//  private def createScreen(
//      config: app.config.AppConfig
//  ): Resource[IO, com.googlecode.lanterna.screen.Screen] = {
//    import app.config.WindowConfig
//    import com.googlecode.lanterna.terminal.swing.SwingTerminalFrame
//
//    Resource.make {
//      IO.delay {
//        val factory =
//          new com.googlecode.lanterna.terminal.DefaultTerminalFactory()
//        val WindowConfig(width, height) =
//          config.initialSize.getOrElse(WindowConfig(120, 40))
//        factory
//          .setTerminalEmulatorTitle(config.title.getOrElse("BAM Editor"))
//          .setInitialTerminalSize(new TerminalSize(width, height))
//        val terminal = factory.createTerminal()
//        terminal match {
//          case frame: SwingTerminalFrame => frame.setLocationRelativeTo(null)
//          case _                         => ()
//        }
//        val screen = new com.googlecode.lanterna.screen.TerminalScreen(terminal)
//        screen.startScreen()
//        screen
//      }
//    } { screen =>
//      IO.delay(screen.stopScreen())
//    }
//  }
//}

/** Usage examples
  */
//object Examples {
//  def simpleEditor()(using IORuntime): IO[ExitCode] = {
//    val cursorInterval = 500.milliseconds
//    val cursorStyle = CursorConfig.underscore
//
//    for {
//      config <- IO.fromEither(
//        pureconfig.ConfigSource.defaultApplication
//          .load[app.config.AppConfig]
//          .leftMap(failures => new RuntimeException(failures.toString))
//      )
//
//      bufferState <- Ref[IO].of(app.buffer.BufferState.empty)
//
//      // Create simple component
//      component = new BufferComponent(bufferState, cursorStyle)
//
//      // ... rest of setup
//    } yield ExitCode.Success
//  }
//
//  def enhancedEditor()(using IORuntime): IO[ExitCode] = {
//    EnhancedMain.runEnhanced(List.empty)
//  }
//
//  def syntaxHighlightingEditor()(using IORuntime): IO[ExitCode] = {
//    val editorConfig = EditorConfig.vimLike
//
//    for {
//      bufferState <- Ref[IO].of(app.buffer.BufferState.empty)
//
//      component = new SyntaxHighlightingComponent(
//        bufferState,
//        editorConfig,
//        SimpleHighlighter.highlight
//      )
//
//      // ... rest of setup
//    } yield ExitCode.Success
//  }
//}
