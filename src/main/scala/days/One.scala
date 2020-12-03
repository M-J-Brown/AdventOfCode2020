package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import main.Main.Logger

class One(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("one")
  override type IN1 = Int
  override type IN2 = Int

  override def calculatePartOne(input: List[Int]): IO[String] = IO {
    (for {
      a <- input
      b <- input
      c <- input
      if a + b + c == 2020
    } yield a * b * c).distinct
  }.flatMap {
    case head :: Nil => IO(head.toString)
    case head :: tail => log.errorAndThrow(s"Many answers! ${head :: tail}")
    case Nil => log.errorAndThrow("No answers!")
  }

  override def calculatePartTwo(input: List[Int]): IO[String] = IO {
    val (big, small) = input.partition(_ > 1010)
    for {
      b <- big
      s <- small
      if b + s == 2020
    } yield b * s
  }.flatMap {
    case head :: Nil => IO(head.toString)
    case head :: tail => log.errorAndThrow(s"Many answers! ${head :: tail}")
    case Nil => log.errorAndThrow("No answers!")
  }

  override def parsePartOne: String => IO[Int] = s => IO(s.toInt)

  override def parsePartTwo: String => IO[Int] = s => IO(s.toInt)
}
