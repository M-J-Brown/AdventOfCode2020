package days

import cats.effect.{ContextShift, IO}
import days.Four.Passport
import main.Main.Logger

class Six(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("six")
  override type IN1 = String
  override type IN2 = String

  override def calculatePartOne(input: List[String]): IO[String] = IO {
    input.foldLeft(List.empty[String] -> "") { case ((finished, current), line) =>
      line match {
        case "" => (current :: finished) -> ""
        case more => finished -> (current + more)

      }
    }
  }
    .map { case (answers, last) => last :: answers }
    .map(_.map(_.distinct.length))
    .map(_.sum.toString)

  override def calculatePartTwo(input: List[String]): IO[String] = IO {
    input.foldLeft(List.empty[List[String]] -> List.empty[String]) { case ((finished, current), line) =>
      line match {
        case "" => (current :: finished) -> Nil
        case more => finished -> (more :: current)

      }
    }
  }
    .map { case (answers, last) => last :: answers }
    .map(_.map(_.map(_.toSet).fold[Set[Char]](allLetters)(_ intersect _)).map(_.size))
    .map(_.sum.toString)

  private val allLetters: Set[Char] = "abcdefghijklmnopqrstuvwxyz".toSet

  override def parsePartOne: String => IO[String] = IO.pure

  override def parsePartTwo: String => IO[String] = parsePartOne
}


