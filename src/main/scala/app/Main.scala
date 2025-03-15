package app

import app.State.showInstance
import app.sline.Term
import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.language.postfixOps

object Main extends IOApp {

  private def program = Resource.make(Term.createF[IO])(_.close)

  private def loop(terminal: Term[IO], state: State): IO[Unit] =
    for {
      rawInKeys <- terminal.read
      effect   = UserAction.keyStrokesToEffect(rawInKeys)
      newState = state ++ effect
      _ <- terminal.print(newState)
      _ <- loop(terminal, newState)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.use(loop(_, State.empty) >> IO.pure(ExitCode.Success))

}
