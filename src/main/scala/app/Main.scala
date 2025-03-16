package app

import app.State.showInstance
import app.terminal.Term
import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.language.postfixOps
import scala.concurrent.duration.DurationInt

object Main extends IOApp {

  private case class AppConfig()
  
  private def program: Resource[IO, Term[IO]] =
    Resource.make(Term.createF[IO])(_.close)

  def run(args: List[String]): IO[ExitCode] =
    program
      .use { term =>
        term.readStream
          .scan(State.empty) { case (state, keyStroke) =>
            state ++ UserAction.keyStrokeToEffect(keyStroke)
          }
          .takeThrough(_.exitCondition)
          .evalTap(term.print(_))
          .compile
          .drain
          .as(ExitCode.Success)
      }
      .handleError(_ => ExitCode.Error)

}
