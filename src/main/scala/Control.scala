import cats.effect.{ExitCode, IO, IOApp}

object Control extends IOApp {
  private def day(day: Day): IO[Unit] =
    (for {
      a <- day.one
      b <- day.two
    } yield a -> b).attempt.flatMap {
      case Left(e) =>  IO(println(s"Error in Day $day ($e)"))
      case Right((a, b)) => IO(println(s"Finished Day $day: PartOne: $a, PartTwo: $b"))
    }

  override def run(args: List[String]): IO[ExitCode] =
    day(One).as(ExitCode.Success)
}
