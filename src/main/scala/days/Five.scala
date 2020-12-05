package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import main.Main.Logger

class Five(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("five")
  override type IN1 = (Int, Int)
  override type IN2 = (Int, Int)

  override def calculatePartOne(input: List[(Int, Int)]): IO[String] = IO {
    input.map { case (row, seat) => 8 * row + seat }.max.toString
  }

  /**
   * Find a missing one, where it +1 and -1 exist.
   */
  override def calculatePartTwo(input: List[(Int, Int)]): IO[String] = {
    def id(rowSeat: (Int, Int)) = 8 * rowSeat._1 + rowSeat._2
    val existing = input.map(id).toSet
    val max = id(127 -> 7)
    val missing = (0 to max).filterNot(existing)
    missing.filter(i => existing(i + 1) && existing(i - 1)).toList match {
      case head :: Nil => IO(head.toString)
      case head :: tail => log.errorAndThrow(s"Many answers! ${head :: tail}")
      case Nil => log.errorAndThrow("No answers!")
    }
  }

  override def parsePartOne: String => IO[(Int, Int)] = s => IO{
    s.splitAt(7).bimap(
      f = rowStr => rowStr.collect {
        case 'F' => '0'
        case 'B' => '1'
      },
      g = seatStr => seatStr.collect {
        case 'L' => '0'
        case 'R' => '1'
      }
    ).bimap(
      f = Integer.parseInt(_, 2),
      g = Integer.parseInt(_, 2)
    )
  }.flatMap {
    case (row, seat) if row < 128 && row >= 0 && seat < 8 && row >= 0 => IO.pure(row -> seat)
    case (row, seat) => log.errorAndThrow(s"Got row:$row and seat:$seat for $s")
  }

  override def parsePartTwo: String => IO[(Int, Int)] = parsePartOne
}
