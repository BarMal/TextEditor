package app.buffer.rope

enum SearchState:
  case Found(index: Int)
  case Poll
  case PollAndPrune

