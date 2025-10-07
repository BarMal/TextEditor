package app.gui

import app.algebra.{LayoutAlgebra, WindowEvent, WindowEventAlgebra}
import cats.effect.kernel.Ref
import cats.effect.{Concurrent, Resource, Sync, Temporal}
import cats.syntax.all.*
import com.googlecode.lanterna.TerminalPosition
import fs2.{Pipe, Stream}

import scala.concurrent.duration.*

/** Enhanced window event handler with proper effect composition and resource
  * management
  */
class WindowEventHandler[F[_]: Temporal](
    eventAlg: WindowEventAlgebra[F],
    layoutAlg: LayoutAlgebra[F]
) { // Simplified to just use Temporal which includes Concurrent
  private def createEventProcessor: Resource[F, Ref[F, WindowState]] =
    Resource.eval(Ref.of[F, WindowState](WindowState.initial))

  // Pipe for processing window resize events
  private def resizeProcessor: Pipe[F, WindowEvent.Resized, Unit] =
    _.evalMap { case WindowEvent.Resized(_, newSize) =>
      for {
        isValid <- layoutAlg.validateLayout(newSize)
        _       <- Temporal[F].whenA(isValid)(layoutAlg.updateLayout(newSize))
      } yield ()
    }

  // Pipe for processing window move events with debouncing
  private def moveProcessor: Pipe[F, WindowEvent.Moved, Unit] =
    _.debounce(100.milliseconds)
      .evalMap { case WindowEvent.Moved(_, pos: TerminalPosition) =>
        layoutAlg.handleWindowMove((pos.getColumn, pos.getRow))
      }

  // Main event processing stream with proper error handling and resource management
  def handleEvents: Stream[F, Unit] =
    Stream.resource(createEventProcessor).flatMap { stateRef =>
      eventAlg.events
        .broadcastThrough(
          // Split event stream by type and handle each type separately
          _.collect { case e: WindowEvent.Resized => e }
            .through(resizeProcessor),
          _.collect { case e: WindowEvent.Moved => e }.through(moveProcessor),
          _.collect { case WindowEvent.Input(_) =>
            Temporal[F].unit // Delegate input handling to the buffer system
          }.evalMap(identity),
          _.collect { case WindowEvent.UnhandledInput(_) =>
            Temporal[F].unit // Ignore unhandled inputs for now
          }.evalMap(identity)
        )
        .onError { case e => Stream.raiseError(e) }
    }
}

// State management for window events
private case class WindowState(
    lastSize: Option[com.googlecode.lanterna.TerminalSize],
    lastPosition: Option[(Int, Int)]
)

private object WindowState {
  def initial: WindowState = WindowState(None, None)
}
