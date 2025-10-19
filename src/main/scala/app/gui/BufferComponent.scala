package app.gui

import app.buffer.BufferState
import app.config.{CursorConfig, EditorConfig}
import cats.effect.kernel.Ref
import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.gui2.Interactable.Result
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.{TerminalPosition, TerminalSize, TextColor}

import java.util.concurrent.atomic.AtomicBoolean

/** A Lanterna component that displays and edits a Rope-based buffer with a
  * customizable blinking cursor
  */
class BufferComponent(
    stateRef: Ref[IO, BufferState],
    cursorConfig: CursorConfig = CursorConfig.default
)(using runtime: IORuntime)
    extends AbstractInteractableComponent[BufferComponent] {

  // Tracks whether the cursor is visible (for blinking)
  private val cursorVisible = new AtomicBoolean(true)

  /** Called by external FS2 tick stream to toggle cursor visibility */
  def onBlinkTick(): Unit = {
    cursorVisible.set(!cursorVisible.get())
    invalidate() // Request repaint
  }

  /** Handles keystrokes and updates BufferState using your existing effects */
  override def handleKeyStroke(key: KeyStroke): Result = {
    println(s"Capturing input $key")
    stateRef.update(_ ++ key).unsafeRunSync()
    cursorVisible.set(true) // Show cursor immediately after input
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
        val g = graphics

        // Set colors
        g.setBackgroundColor(cursorConfig.backgroundColor)
        g.setForegroundColor(cursorConfig.textColor)
        g.fill(' ')

        // Get current buffer state
        val state   = stateRef.get.unsafeRunSync()
        val content = state.buffer.collect()
        val lines = content.split("\n", -1) // -1 to keep trailing empty strings

        val width  = g.getSize.getColumns
        val height = g.getSize.getRows

        // Calculate which line the cursor is on
        val textBeforeCursor = content.take(state.cursorPosition)
        val cursorLine       = textBeforeCursor.count(_ == '\n')
        val cursorColumn =
          state.cursorPosition - textBeforeCursor.lastIndexOf('\n') - 1

        // Draw visible lines
        val startLine    = Math.max(0, cursorLine - height / 2)
        val visibleLines = lines.slice(startLine, startLine + height)

        visibleLines.zipWithIndex.foreach { case (line, idx) =>
          val y = idx
          if (y < height) {
            // Draw line content
            val displayLine = line.take(width)
            g.putString(0, y, displayLine)

            // Draw cursor if on this line
            if (startLine + idx == cursorLine && cursorVisible.get()) {
              val cursorX = Math.min(cursorColumn, width - 1)

              // Get character at cursor position (or space if at end of line)
              val charAtCursor = if (cursorColumn < line.length) {
                line.charAt(cursorColumn)
              } else {
                ' '
              }

              // Draw cursor
              val cursorChar = if (cursorConfig.showCharacterUnderCursor) {
                charAtCursor
              } else {
                cursorConfig.cursorChar
              }

              g.setBackgroundColor(cursorConfig.cursorBackgroundColor)
              g.setForegroundColor(cursorConfig.cursorForegroundColor)
              g.setCharacter(cursorX, y, cursorChar)

              // Reset colors for rest of drawing
              g.setBackgroundColor(cursorConfig.backgroundColor)
              g.setForegroundColor(cursorConfig.textColor)
            }
          }
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
