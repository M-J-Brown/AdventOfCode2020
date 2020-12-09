package days

import cats.effect.{ContextShift, IO}
import main.Main.Logger

import scala.annotation.tailrec

class Nine(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("nine")
  override type IN1 = Long
  override type IN2 = Long

  override def calculatePartOne(input: List[Long]): IO[String] = IO {
    input.sliding(26).collect {
      case list if list.size == 26 => list.init -> list.last
    }
    .collect {
      case (twentyFive, twentySix) if !twentyFive.combinations(2).map(_.sum).contains(twentySix) => twentySix
    }
  }.flatMap(_.toList match {
    case head :: Nil => IO(head.toString)
    case head :: tail => log.errorAndThrow(s"Many answers! ${head :: tail}")
    case Nil => log.errorAndThrow("No answers!")
  })

  override def calculatePartTwo(input: List[Long]): IO[String] = {
    def tryIt(input: List[Long], target: Long): (Long, List[Long]) =
      input.foldLeft(0L -> List.empty[Long]){ case ((total, acc), next) =>
        if (total >= target)
          total -> acc
        else
          (total + next) -> (next :: acc)
        }

    @tailrec
    def recurse(input: List[Long], target: Long): List[Long] = {
      if (input.isEmpty)
        Nil
      else
        tryIt(input, target) match {
          case (sum, acc) if sum == target => acc
          case _ => recurse(input.tail, target)
        }
    }

    (for {
      target <- calculatePartOne(input).map(_.toLong)
      answers = recurse(input, target)
    } yield answers)
      .map(l => l.minOption -> l.maxOption)
      .flatMap {
        case (Some(min), Some(max)) if  min != max => IO.pure((min + max).toString)
        case other => log.errorAndThrow(s"No solution! Min and Max were $other")
      }
  }

  override def parsePartOne: String => IO[Long] = s => IO(s.toLong)

  override def parsePartTwo: String => IO[Long] = parsePartOne

}


