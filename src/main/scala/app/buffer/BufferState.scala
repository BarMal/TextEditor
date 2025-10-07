package app.buffer

import app.*
import app.UserInput.flatten
import app.action.Effect
import app.action.editor.{DeleteEffect, NavigateEffect, StateChangeEffect, WriteEffect}
import app.buffer.WriteMode.Write
import app.buffer.rope.{Rope, Balance}
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
    userEffects: Vector[Effect],
    lineLength: Int,
    selected: TogglingSet[Int],
    writeMode: WriteMode,
    currentFormatting: Set[Formatting],
    formattingMap: Map[Int, Set[Formatting]],
    viewportSize: Int
) extends Focusable[BufferState] {

  def withViewportSize(size: Int): BufferState =
    this.copy(viewportSize = size, lineLength = size / 2)

  override def ++(in: KeyStroke): BufferState =
    UserInput.keyStrokeToEffect(in.flatten) match
      case effect: WriteEffect       => effect.effect(this)
      case effect: DeleteEffect      => effect.effect(this)
      case effect: NavigateEffect    => effect.effect(this)
      case effect: StateChangeEffect => effect.effect(this)
      case others =>
        this.copy(writeMode = Write, userEffects = others +: userEffects)
}

object BufferState {

  given Balance =
    Balance(
      weightBalance = 64,
      heightBalance = 8,
      leafChunkSize = 128
    )

  def empty =
    new BufferState(
      buffer = Rope.mobyDick,
      cursorPosition = 0,
      userEffects = Vector.empty[Effect],
      lineLength = 50,
      selected = TogglingSet.empty[Int],
      writeMode = Write,
      currentFormatting = Set.empty[Formatting],
      formattingMap = Map.empty[Int, Set[Formatting]],
      viewportSize = 4096
    )
}

extension (state: BufferState)
  def withContent(newContent: String)(using balance: Balance): BufferState = {
    val oldContent = state.buffer.collect()
    val newRope = Rope(newContent)
    val minLen = math.min(oldContent.length, newContent.length)
    val firstDiff = (0 until minLen).find(i => oldContent.charAt(i) != newContent.charAt(i)).getOrElse(minLen)
    val newCursor = math.min(firstDiff + (newContent.length - oldContent.length).max(0), newRope.weight)
    state.copy(
      buffer = newRope,
      cursorPosition = newCursor,
      selected = app.buffer.TogglingSet.empty,
      formattingMap = Map.empty
    )
  }
