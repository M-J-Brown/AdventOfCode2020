package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import main.Main.Logger

import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec

class Ten(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("ten")
  override type IN1 = Long
  override type IN2 = Long

  override def calculatePartOne(input: List[Long]): IO[String] = {
    val init = 0L
    val device = input.max + 3
    val all = (init :: device :: input).sorted
    val diffs = all.sliding(2).map(pair => pair.lastOption.getOrElse(0L) - pair.headOption.getOrElse(0L)).toList
    if (diffs.forall(_ <= 3))
      IO.pure(diffs.count(_ == 1) * diffs.count(_ == 3)).map(_.toString)
    else
      log.errorAndThrow(s"Uh-Oh: had to jump by more than 3: ${all.zip(diffs)}")
  }

  /**
   * Naive recursive solution gets stuck because there are so many paths to each number
   * Doing it in reverse order means that all the numbers you need to use are precalculated
   */
  override def calculatePartTwo(input: List[Long]): IO[String] = {
    val init = 0L
    val device = input.max + 3
    val all = (init :: device :: input).sorted
    val validConnections = all.map(adapter => adapter -> all.filter(a => a > adapter && a <= adapter + 3)).toMap

    val precalculated: Map[Long, Long] = all.foldRight(Map.empty[Long, Long]){ case (next, acc) =>
      //These are the valid connections. They must all be greater, and we're in reverse order, so get them out the map
      val connections = validConnections.getOrElse(next, Nil)
      val counts = connections.map(acc.getOrElse(_, 0L))

      //If this list is empty, I am the final number - put a 1 in the map
      val sum = if (counts.isEmpty) 1L else counts.sum
      acc + (next -> sum)
    }
    IO.pure(precalculated(0)).map(_.toString)
  }

  override def parsePartOne: String => IO[Long] = s => IO(s.toLong)

  override def parsePartTwo: String => IO[Long] = parsePartOne

}


