import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import days.{Day, One, Three, Two}

object Main extends IOApp {
  private def day(day: Day): IO[Unit] = {
    for {
      _ <- day.one.attempt.flatMap {
        case Left(e) => IO(println(s"Error in day $day pt 1: $e"))
        case Right(a) => IO(println(s"Finished day $day pt 1 : $a"))
      }
      _ <- day.two.attempt.flatMap {
        case Left(e) => IO(println(s"Error in day $day pt 2: $e"))
        case Right(b) => IO(println(s"Finished day $day pt 2 : $b"))
      }
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    days.map(day).sequence.as(ExitCode.Success)
  }

  val days: List[Day] = List(One, Two, Three)
}
