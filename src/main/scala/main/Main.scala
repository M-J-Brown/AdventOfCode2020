package main

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import days._

object Main extends IOApp {

  trait Logger[F[_]] {
    def info(msg: String): F[Unit]

    def errorAndThrow(msg: String): F[Nothing]
  }

  object Logger {
    implicit val ioLogger: Logger[IO] = new Logger[IO] {
      override def info(msg: String): IO[Unit] = IO(println(msg))

      override def errorAndThrow(msg: String): IO[Nothing] = IO(Console.err.println(msg)) *> IO.raiseError(new RuntimeException(msg))
    }

    def apply[F[_]: Logger]: Logger[F] = implicitly
  }

  private def day(day: Day)(implicit logger: Logger[IO]): IO[Unit] = {
    for {
      _ <- day.one.attempt.flatMap {
        case Left(e) => logger.info(s"Error in day $day pt 1: $e")
        case Right(a) => logger.info(s"Finished day $day pt 1 : $a")
      }
      _ <- day.two.attempt.flatMap {
        case Left(e) => logger.info(s"Error in day $day pt 2: $e")
        case Right(b) => logger.info(s"Finished day $day pt 2 : $b")
      }
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    days.map(day).sequence.as(ExitCode.Success)
  }

  val days: List[Day] = List(new One, new Two, new Three)
}
