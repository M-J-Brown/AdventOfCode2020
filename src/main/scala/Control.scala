import cats.effect.{ExitCode, IO, IOApp}

object Control extends IOApp {
  private def day(day: Day): IO[Unit] =
    (for {
      _ <- day.one
      _ <- day.two
    } yield ()).attempt.flatMap {
      case Left(e) =>  IO(println(s"Error in Day $day ($e)"))
      case Right(()) => IO(println(s"Finished Day $day"))
    }

  override def run(args: List[String]): IO[ExitCode] =
    day(One).as(ExitCode.Success)
}
