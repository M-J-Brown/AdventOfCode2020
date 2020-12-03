package days

import cats.effect.{ContextShift, IO}
import cats.implicits._

object One extends Day {
  override val name: String = "one"
  override type IN1 = Int
  override type IN2 = Int

  override def calculatePartOne(input: List[Int])(implicit cs: ContextShift[IO]): IO[String] = IO {
    (for {
      a <- input
      b <- input
      c <- input
      if a + b + c == 2020
    } yield a * b * c).distinct
  }.flatMap {
    case head :: Nil => IO(head.toString)
    case head :: tail => IO.raiseError(new RuntimeException(s"Many answers! ${head :: tail}"))
    case Nil => IO.raiseError(new RuntimeException("No answers!"))
  }

  override def calculatePartTwo(input: List[Int])(implicit cs: ContextShift[IO]): IO[String] = IO {
    val (big, small) = input.partition(_ > 1010)
    for {
      b <- big
      s <- small
      if b + s == 2020
    } yield b * s
  }.flatMap {
    case head :: Nil => IO(head.toString)
    case head :: tail => IO.raiseError(new RuntimeException(s"Many answers! ${head :: tail}"))
    case Nil => IO.raiseError(new RuntimeException("No answers!"))
  }

  override def parsePartOne: String => IO[Int] = s => IO(s.toInt)

  override def parsePartTwo: String => IO[Int] = s => IO(s.toInt)
}
