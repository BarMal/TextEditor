package app.buffer

import app.UserInput.flatten
import WriteMode.Write
import app.action.Effect
import app.action.editor.{
  DeleteEffect,
  NavigateEffect,
  StateChangeEffect,
  WriteEffect
}
import app.buffer.rope.{Balance, Rope}
import app.*
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input.KeyStroke

//case class State(
//    bufferState: BufferState,
//    menuState: MenuState,
//    inFocus: Focusable[?]
//) {
//
//  def ++(in: KeyStroke): State = inFocus match
//    case state: MenuState   => this.copy(menuState = state)
//    case state: BufferState => this.copy(bufferState = state)
//
//}

sealed trait Focusable[T <: Focusable[T]] {
  def ++(in: KeyStroke): T
}

case class MenuState() extends Focusable[MenuState] {

  override def ++(in: KeyStroke): MenuState = this
}

enum WriteMode:
  case Write
  case Overwrite

object WriteMode {
  def flip(mode: WriteMode): WriteMode = mode match
    case Write     => Overwrite
    case Overwrite => Write
}

case class BufferState(
    buffer: Rope,
    cursorPosition: Int,
    userEffects: List[Effect],
    lineLength: Int,
    selected: TogglingSet[Int],
    writeMode: WriteMode,
    currentFormatting: Set[Formatting],
    formattingMap: Map[Int, Set[Formatting]]
) extends Focusable[BufferState] {

  override def ++(in: KeyStroke): BufferState =
    UserInput.keyStrokeToEffect(in.flatten) match
      case effect: WriteEffect       => effect.effect(this)
      case effect: DeleteEffect      => effect.effect(this)
      case effect: NavigateEffect    => effect.effect(this)
      case effect: StateChangeEffect => effect.effect(this)
      case others =>
        BufferState(
          buffer = buffer,
          cursorPosition = cursorPosition,
          userEffects = others :: userEffects,
          lineLength = lineLength,
          selected = selected,
          writeMode = Write,
          currentFormatting = currentFormatting,
          formattingMap = formattingMap
        )
}

object BufferState {

  given balance: Balance =
    Balance(weightBalance = 32, heightBalance = 10, leafChunkSize = 32)

  def empty =
    new BufferState(
      buffer = Rope.mobyDick,
      cursorPosition = 0,
      userEffects = List.empty[Effect],
      lineLength = 50,
      selected = TogglingSet.empty[Int],
      writeMode = Write,
      currentFormatting = Set.empty[Formatting],
      formattingMap = Map.empty[Int, Set[Formatting]]
    )

}
