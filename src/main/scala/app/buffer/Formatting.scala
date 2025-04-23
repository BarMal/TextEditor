package app.buffer

import com.googlecode.lanterna.TextColor.ANSI

enum Formatting:
  case Bold
  case Italic
  case Underscore
  case Inverted
  case Foreground(value: ANSI)
  case Background(value: ANSI)