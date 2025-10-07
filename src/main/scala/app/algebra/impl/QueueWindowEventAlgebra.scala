package app.algebra.impl

import app.algebra.{WindowEvent, WindowEventAlgebra}
import cats.effect.Async
import cats.effect.std.Queue
import fs2.Stream

class QueueWindowEventAlgebra[F[_]: Async](queue: Queue[F, WindowEvent]) extends WindowEventAlgebra[F] {
  def events: Stream[F, WindowEvent] = Stream.fromQueueUnterminated(queue)
  def publish(event: WindowEvent): F[Unit] = queue.offer(event)
}
