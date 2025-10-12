package app

import app.algebra.*
import app.algebra.impl.*
import app.buffer.BufferState
import app.buffer.BufferState.given
import app.config.{AppConfig, Title, WindowConfig}
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
import com.googlecode.lanterna.input.KeyStroke

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Try

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private def createScreen[F[_]: Sync](
      configAlg: ConfigAlgebra[IO]
  ): Resource[IO, Screen] =
    Resource.make {
      for {
        config <- configAlg.loadConfig
        screen <- Sync[IO].delay {
          val factory: DefaultTerminalFactory =
            new DefaultTerminalFactory().setTerminalEmulatorTitle(
              config.title.getOrElse(Title.default).toString
            )
          val WindowConfig(width, height) =
            config.initialSize.getOrElse(WindowConfig.default)
          factory.setInitialTerminalSize(new TerminalSize(width, height))
          val term = factory.createTerminal()
          new TerminalScreen(term)
        }
      } yield screen
    } { screen =>
      Sync[IO].delay(screen.stopScreen(true))
    }

  private def in(
      screen: Screen,
      stateRef: Ref[IO, BufferState]
  ): fs2.Stream[IO, Unit] =
    fs2.Stream
      .repeatEval(
        IO.fromTry(Try(screen.pollInput()))
          .map(Option(_))
          .handleErrorWith(err => logger.error(err)("Input failure").as(None))
      )
      .collect { case Some(key) => key }
      .evalMap(keyStroke =>
        stateRef
          .getAndUpdate(_ ++ keyStroke)
          .void
      )

  private def out(
      renderAlg: RenderAlgebra[IO],
      stateRef: Ref[IO, BufferState]
  ): fs2.Stream[IO, Unit] =
    fs2.Stream
      .constant(System.currentTimeMillis())
      .metered[IO](16 milliseconds)
      .evalMap(_ => stateRef.get.flatMap(renderAlg.render))

  private def process(
      screen: Screen,
      renderAlg: RenderAlgebra[IO],
      stateRef: Ref[IO, BufferState]
  ): IO[ExitCode] =
    out(renderAlg, stateRef)
      .concurrently(in(screen, stateRef))
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
      val layoutAlg: LanternaLayoutAlgebra[IO] =
        new LanternaLayoutAlgebra[IO](panel, filePane, editor, lastKnownSize)

      // Set window hints
      import scala.jdk.CollectionConverters.*
      val hints = Set(Window.Hint.FIXED_POSITION, Window.Hint.EXPANDED)
      window.setHints(hints.asJava)

      window.setComponent(panel)
      window.addWindowListener(new EditorWindowListener(eventAlg))

      val eventHandler = new WindowEventHandler[IO](eventAlg, layoutAlg)
      (window, eventHandler)
    }

  override def run(args: List[String]): IO[ExitCode] =
    createScreen[IO](new PureConfigAlgebra[IO])
      .use { screen =>
        for {
          _ <- logger.info("Starting BAM text editor")
          textGUI   = new MultiWindowTextGUI(screen)
          writer    = new ScreenWriter[IO](screen)
          renderAlg = new LanternaRenderAlgebra[IO](writer)

          bufferRef <- Ref[IO].of(BufferState.empty)

          // Create editor components
          components <- createEditor[IO](bufferRef)
          (editor, panel, filePane) = components
          _ <- editor.initialize()

          // Setup window and event handling
          (window, eventHandler) <- setupWindowAndHandlers[IO](
            editor,
            panel,
            filePane,
            bufferRef
          )

          _          <- screen.startScreen()
          eventFiber <- eventHandler.handleEvents.compile.drain.start

          _ <- {
            window.getComponent.invalidate()
            textGUI.updateScreen()
            textGUI.addWindowAndWait(window)
          }.pure[F]

          exitCode <- process(screen, renderAlg, bufferRef)

          _ <- eventFiber.cancel
        } yield exitCode
      }
      .handleErrorWith { error =>
        logger.error(error)("Error in main run loop").as(ExitCode.Error)
      }
}

extension [T, F[_]: Sync](underlying: T) {
  def withF(setters: (T => ?)*): F[T] =
    Sync[F].delay(setters.toList.map(set => set(underlying))).as(underlying)
}

//extension [F[_]: Async](stream: fs2.Stream[F, ?]) {
//  def void: fs2.Stream[F, Unit] = stream.as(())
//}
