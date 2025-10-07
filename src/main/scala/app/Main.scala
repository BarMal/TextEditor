package app

import app.algebra.*
import app.algebra.impl.*
import app.buffer.BufferState
import app.buffer.BufferState.given
import app.config.AppConfig
import app.gui.{EditorTextBox, EditorWindowListener, WindowEventHandler}
import app.screen.ScreenWriter
import cats.data.Kleisli
import cats.effect.*
import cats.effect.std.Queue
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.syntax.all.*
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import cats.effect.unsafe.implicits.global

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object Main extends IOApp {
  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private val configAlg = new PureConfigAlgebra[IO]

  private def createScreen[F[_]: Sync](
      configAlg: ConfigAlgebra[IO]
  ): Resource[IO, Screen] =
    Resource.make {
      for {
        title       <- configAlg.getEditorTitle
        initialSize <- configAlg.getInitialSize
        screen <- Sync[IO].delay {
          val factory =
            new DefaultTerminalFactory().setTerminalEmulatorTitle(title)
          initialSize.foreach { case (w, h) =>
            factory.setInitialTerminalSize(new TerminalSize(w, h))
          }
          val term = factory.createTerminal()
          new TerminalScreen(term)
        }
      } yield screen
    } { screen =>
      Sync[IO].delay(screen.stopScreen())
    }

  private val bufferStateRef: IO[Ref[IO, BufferState]] =
    Ref.of[IO, BufferState](BufferState.empty)

  private val lastBufferStateHashRef: IO[Ref[IO, Option[Int]]] =
    Ref.of[IO, Option[Int]](None)

  private def in(
      screenAlg: ScreenAlgebra[IO],
      stateRef: Ref[IO, BufferState],
      lastHashRef: Ref[IO, Option[Int]]
  ): fs2.Stream[IO, Unit] =
    screenAlg.readInput
      .evalMap { keyStroke =>
        for {
          lastState <- stateRef.getAndUpdate(_ ++ keyStroke)
          _         <- lastHashRef.update(_ => Some(lastState.hashCode()))
        } yield ()
      }

  private def out(
      renderAlg: RenderAlgebra[IO],
      stateRef: Ref[IO, BufferState],
      lastStateRef: Ref[IO, Option[Int]]
  ): fs2.Stream[IO, Unit] =
    fs2.Stream
      .constant(System.currentTimeMillis())
      .metered[IO](16 milliseconds)
      .evalMap(_ =>
        for {
          state         <- stateRef.get
          lastStateHash <- lastStateRef.get
          _ <- IO.unlessA(lastStateHash.contains(state.hashCode()))(
            renderAlg.render(state)
          )
        } yield ()
      )

  private def process(
      screenAlg: ScreenAlgebra[IO],
      renderAlg: RenderAlgebra[IO],
      stateRef: Ref[IO, BufferState],
      lastHashState: Ref[IO, Option[Int]]
  ): IO[ExitCode] =
    out(renderAlg, stateRef, lastHashState)
      .concurrently(in(screenAlg, stateRef, lastHashState))
      .compile
      .drain
      .as(ExitCode.Success)
      .handleErrorWith(
        logger
          .error(_)("Something went wrong with the stream")
          .as(ExitCode.Error)
      )

  private def createEditor[F[_]: Sync](
      bufferStateRef: Ref[IO, BufferState]
  ): IO[(EditorTextBox[IO], Panel, Panel)] =
    Sync[IO].delay {
      val textBox  = new TextBox()
      val filePane = new Panel()
      val panel    = new Panel()

      // Initialize UI components
      filePane.addComponent(new Label("File Pane"))
      val gridLayout = new GridLayout(2)
      gridLayout.setHorizontalSpacing(1)
      gridLayout.setVerticalSpacing(0)
      gridLayout.setLeftMarginSize(0)
      gridLayout.setRightMarginSize(0)
      panel.setLayoutManager(gridLayout)

      // Create algebras
      val bufferAlg   = new RefBufferAlgebra[IO](bufferStateRef)
      val editorUIAlg = new LanternaEditorUIAlgebra[IO](textBox)
      val editor      = new EditorTextBox[IO](bufferAlg, editorUIAlg)

      (editor, panel, filePane)
    }

  private def setupWindowAndHandlers[F[_]](
      editor: EditorTextBox[IO],
      panel: Panel,
      filePane: Panel,
      eventAlg: WindowEventAlgebra[IO],
      bufferRef: Ref[F, BufferState]
  )(using
      F: Sync[IO],
      runtime: IORuntime
  ): IO[(BasicWindow, WindowEventHandler[IO])] =
    F.delay {
      val window = new BasicWindow("Bam Editor")
      val lastKnownSize =
        new AtomicReference[TerminalSize](new TerminalSize(100, 30))

      // Create layout algebra
      val layoutAlg: LanternaLayoutAlgebra[IO] = new LanternaLayoutAlgebra[IO](panel, filePane, editor, lastKnownSize)

      // Set window hints
      import scala.jdk.CollectionConverters.*
      val hints = Set(Window.Hint.FIXED_POSITION, Window.Hint.EXPANDED)
      window.setHints(hints.asJava)

      window.setComponent(panel)
      window.addWindowListener(new EditorWindowListener(eventAlg))

      val eventHandler = new WindowEventHandler[IO](eventAlg, layoutAlg)
      (window, eventHandler)
    }

  private val initEditor: Kleisli[
    IO,
    (Ref[IO, BufferState], WindowEventAlgebra[IO]),
    (EditorTextBox[IO], BasicWindow)
  ] =
    Kleisli { case (bufferRef, eventAlg) =>
      for {
        // Create editor and components
        components <- createEditor[IO](bufferRef)
        (editor, panel, filePane) = components

        // Initialize editor
        _ <- editor.initialize()

        // Setup window
        (window, handler) <- setupWindowAndHandlers[IO](
          editor,
          panel,
          filePane,
          eventAlg,
          bufferRef
        )
      } yield (editor, window)
    }

  private val startSyncFiber
      : Kleisli[IO, EditorTextBox[IO], Fiber[IO, Throwable, Unit]] =
    Kleisli { editor =>
      fs2.Stream
        .awakeEvery[IO](
          100.milliseconds
        ) // Increased sync frequency for better responsiveness
        .evalMap(_ =>
          editor
            .syncFromBuffer()
            .handleErrorWith(e =>
              logger.error(e)("Error during buffer sync").as(())
            )
        )
        .compile
        .drain
        .start
    }

  private val runGui: Kleisli[IO, (GUIAlgebra[IO], BasicWindow), Unit] =
    Kleisli { case (guiAlg, window) =>
      for {
        _ <- guiAlg.invalidateComponent(window)
        _ <- guiAlg.updateScreen
        _ <- guiAlg.addWindow(window)
      } yield ()
    }

  override def run(args: List[String]): IO[ExitCode] =
    createScreen[IO](configAlg)
      .use { screen =>
        given runtime: IORuntime =
          runtime // Make runtime available for setupWindow

        for {
          // Create event handling infrastructure
          eventQueue <- Queue.unbounded[IO, WindowEvent]
          eventAlg = new QueueWindowEventAlgebra[IO](eventQueue)

          _ <- IO.delay(screen.startScreen())
          screenAlg = new LanternaScreenAlgebra[IO](screen)
          textGUI   = new MultiWindowTextGUI(screen)
          guiAlg    = new LanternaGUIAlgebra[IO](textGUI)
          bufferRef <- Ref[IO].of(BufferState.empty)
          hashRef   <- Ref[IO].of(Option.empty[Int])
          writer    = new ScreenWriter[IO](screen)
          renderAlg = new LanternaRenderAlgebra[IO](writer)

          // Create editor components
          components <- createEditor[IO](bufferRef)
          (editor, panel, filePane) = components
          _ <- editor.initialize()

          // Setup window and event handling
          setup <- setupWindowAndHandlers[IO](
            editor,
            panel,
            filePane,
            eventAlg,
            bufferRef
          )
          (window, eventHandler) = setup

          // Start all concurrent processes
          syncFiber  <- startSyncFiber.run(editor)
          eventFiber <- eventHandler.handleEvents.compile.drain.start

          _        <- runGui.run((guiAlg, window))
          exitCode <- process(screenAlg, renderAlg, bufferRef, hashRef)

          // Cleanup
          _ <- syncFiber.cancel
          _ <- eventFiber.cancel
        } yield exitCode
      }
      .handleErrorWith { error =>
        logger.error(error)("Error in main run loop").as(ExitCode.Error)
      }
}

extension [F[_]: Async](stream: fs2.Stream[F, ?]) {
  def void: fs2.Stream[F, Unit] = stream.as(())
}
