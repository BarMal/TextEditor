package app.buffer.rope

sealed trait SearchState

object SearchState {
  case class Found(index: Int) extends SearchState

  sealed trait NotFound extends SearchState

  case object Poll extends NotFound

  case object PollAndPrune extends NotFound
}
