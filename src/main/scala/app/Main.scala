package app

import app.buffer.BufferState
import app.config.{AppConfig, WindowConfig}
import cats.effect.*
import cats.syntax.all.*
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.graphics.Theme
import com.googlecode.lanterna.gui2.dialogs.MessageDialogBuilder
import com.googlecode.lanterna.gui2.{
  BasicWindow,
  Component,
  DefaultWindowManager,
  GridLayout,
  Label,
  MultiWindowTextGUI,
  Panel,
  TextBox,
  Window
}
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.swing.SwingTerminalFrame
import com.googlecode.lanterna.terminal.{
  AbstractTerminal,
  DefaultTerminalFactory,
  ExtendedTerminal,
  IOSafeTerminal,
  Terminal,
  TerminalFactory
}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import pureconfig.ConfigSource

import scala.concurrent.duration.DurationInt
import scala.util.Try
import scala.jdk.CollectionConverters.*
import scala.language.reflectiveCalls

object Main extends IOApp {

  private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jFactory.create[IO].getLogger

  private def loadConfig: IO[AppConfig] =
    IO.fromEither(
      ConfigSource.defaultApplication
        .load[AppConfig]
        .leftMap(failures => new RuntimeException(failures.toString))
    )

  private def createWindow(config: AppConfig): Resource[IO, Window] =
    Resource.make(
      IO.delay(new BasicWindow(config.title.getOrElse("BAM Editor")))
    )(window => IO.delay(window.waitUntilClosed()))

  private def screenFactory(config: AppConfig): Screen = {
    val factory: DefaultTerminalFactory = new DefaultTerminalFactory()
    val WindowConfig(width, height) =
      config.initialSize.getOrElse(WindowConfig(80, 60))
    val title: String = config.title.getOrElse("BAM Editor")
    factory
      .setTerminalEmulatorTitle(title)
      .setInitialTerminalSize(new TerminalSize(width, height))
    val terminal: Terminal = factory.createTerminal()
    terminal match {
      case frame: SwingTerminalFrame => frame.setLocationRelativeTo(null)
      case _                         => ()
    }
    new TerminalScreen(terminal)
  }

  private def createScreen(config: AppConfig): Resource[IO, Screen] =
    Resource.make {
      IO.delay(screenFactory(config))
        .flatTap(screen => IO.delay(screen.startScreen()))
    } { screen =>
      IO.delay(screen.stopScreen())
    }

  private def inputStream(
      screen: Screen,
      stateRef: Ref[IO, BufferState]
  ): fs2.Stream[IO, Unit] =
    fs2.Stream
      .repeatEval(
        IO.fromTry(Try(screen.pollInput()))
          .map(Option(_))
          .handleErrorWith(err => logger.error(err)("Input failure").as(None))
      )
      .evalTap(_.fold(logger.error("None keystroke registered"))(
        keyStroke => logger.info(s"KeyStroke: $keyStroke")
      )
      )
      .collect { case Some(key) => key }
      .evalMap(keyStroke => stateRef.update(_ ++ keyStroke))

  private def outputStream(
      bufferLabel: Label,
      gui: MultiWindowTextGUI,
      stateRef: Ref[IO, BufferState]
  ): fs2.Stream[IO, Unit] =
    fs2.Stream
      .fixedRate[IO](16.milliseconds)
      .evalMap(_ =>
        for {
          state <- stateRef.get
          _ = bufferLabel.setText(state.buffer.collect())
          _ <- IO.whenA(gui.isPendingUpdate)(IO.delay(gui.updateScreen()))
        } yield ()
      )

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      config <- loadConfig
      exitCode <- createScreen(config).use { screen =>
        for {
          _           <- logger.info("Starting BAM text editor")
          bufferState <- Ref[IO].of(BufferState.empty)
          gui = new MultiWindowTextGUI(screen)
          layoutManager = new GridLayout(2)
            .setHorizontalSpacing(5)
            .setVerticalSpacing(5)
          mainWindow = new BasicWindow("Main Window")
          _ = mainWindow.setHints(
            List(Window.Hint.CENTERED, Window.Hint.FIT_TERMINAL_WINDOW).asJava
          )
//          bufferTextBox    = new TextBox("Hello world")
          bufferLabel      = new Label("Hello world")
          sidePanelTextBox = new TextBox("Goodbye universe")
          mainPanel        = new Panel(layoutManager)
          _                = mainPanel.addComponent(0, sidePanelTextBox)
          _                = mainPanel.addComponent(1, bufferLabel)
          _                = mainWindow.setComponent(mainPanel)
          _                = gui.addWindow(mainWindow)

          exitCode <- outputStream(bufferLabel, gui, bufferState)
            .concurrently(inputStream(screen, bufferState))
            .compile
            .drain
            .as(ExitCode.Success)
            .handleErrorWith(err =>
              logger.error(err)("Error in main loop").as(ExitCode.Error)
            )
        } yield exitCode
      }
    } yield exitCode).handleErrorWith { error =>
      logger.error(error)("Fatal error").as(ExitCode.Error)
    }
}
