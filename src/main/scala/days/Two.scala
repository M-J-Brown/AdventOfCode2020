package days

import cats.data.State
import cats.effect.{ContextShift, IO}
import cats.implicits._

object Two extends Day {
  override val name: String = "two"
  override type IN1 = (Range, Char, String)
  override type IN2 = (Int, Int, Char, String)

  override def calculatePartOne(input: List[(Range, Char, String)])(implicit cs: ContextShift[IO]): IO[String] = IO {
    input.filter { case (range, char, pass) =>
      range.contains(pass.count(_ == char))
    }
  }.map(_.size.toString)

  override def calculatePartTwo(input: List[(Int, Int, Char, String)])(implicit cs: ContextShift[IO]): IO[String] = IO {
    input.filter { case (first, second, char, pass) =>
      val allOcurrences = pass.zipWithIndex.collect {
        case (c, i) if c == char => i + 1
      }
      allOcurrences.contains(first) != allOcurrences.contains(second)
    }
  }.map(_.size.toString)

  override def parsePartOne: String => IO[(Range, Char, String)] = in => IO {
    for {
      first <- stateSplit('-')
      second <- stateSplit(' ')
      char <- stateSplit(':')
      rest <- State.get[String]
    } yield (Range.inclusive(first.toInt, second.toInt), char.charAt(0), rest.trim)
  }.map(_.runA(in).value)

  def stateSplit(char: Char): State[String, String] = State(s => s.splitAt(s.indexOf(char)).map(_.drop(1)).swap)

  override def parsePartTwo: String => IO[(Int, Int, Char, String)] = in => IO {
    for {
      first <- stateSplit('-')
      second <- stateSplit(' ')
      char <- stateSplit(':')
      rest <- State.get[String]
    } yield (first.toInt, second.toInt, char.charAt(0), rest.trim)
  }.map(_.runA(in).value)
}
