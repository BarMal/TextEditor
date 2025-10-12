package app.algebra.impl

import app.algebra.ScreenAlgebra
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.screen.Screen
import cats.effect.Sync
import cats.syntax.functor.*
import fs2.Stream

class LanternaScreenAlgebra[F[_]: Sync](screen: Screen) extends ScreenAlgebra[F] {
  def readInput: Stream[F, KeyStroke] = 
    Stream.repeatEval(
      Sync[F].delay(Option(screen.pollInput()))
    ).collect { case Some(key) => key }

}
