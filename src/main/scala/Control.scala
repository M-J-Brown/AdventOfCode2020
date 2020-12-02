import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Control extends IOApp {
  private def day(day: Day): IO[Unit] =
    (for {
      a <- day.one
      b <- day.two
    } yield a -> b).attempt.flatMap {
      case Left(e) => IO(println(s"Error in Day $day ($e)"))
      case Right((a, b)) => IO(println(s"Finished Day $day: Part One: $a --- Part Two: $b"))
    }

  override def run(args: List[String]): IO[ExitCode] = {
    days.map(day).sequence.as(ExitCode.Success)
  }

  val days: List[Day] = List(One, Two)
}
