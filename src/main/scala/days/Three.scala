package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import main.Main.Logger

class Three(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("three")
  override type IN1 = IndexedSeq[Boolean]
  override type IN2 = IndexedSeq[Boolean]

  /**
   * Right 3, down 1
   */
  override def calculatePartOne(input: List[IndexedSeq[Boolean]]): IO[String] = IO {
    input.foldLeft(0 -> 0) { case ((count, idx), row) =>
      if (row(idx % row.size))
        (count + 1) -> (idx + 3)
      else
        (count) -> (idx + 3)
    }
  }.map(_._1.toString)

  /**
   * Right 1, down 1.
   * Right 3, down 1. (This is the slope you already checked.)
   * Right 5, down 1.
   * Right 7, down 1.
   * Right 1, down 2.
   */
  override def calculatePartTwo(input: List[IndexedSeq[Boolean]]): IO[String] =
    List(1 -> 1, 3 -> 1, 5 -> 1, 7 -> 1, 1 -> 2)
      .map(t => countTrees(input, t._1, t._2))
      .sequence
      .map(_.sortBy(_._1))
      .flatTap(res => log.info(res.map { case (c, n) => s"Path $n has $c trees" }.mkString(",\n")))
      .map(_.map(_._1.toLong).product.toString)

  override def parsePartOne: String => IO[IndexedSeq[Boolean]] = s => s.map {
    case '.' => IO.pure(false)
    case '#' => IO.pure(true)
    case _ => log.errorAndThrow("nope")
  }.toList.sequence.map(_.toIndexedSeq)

  override def parsePartTwo: String => IO[IndexedSeq[Boolean]] = parsePartOne

  private def countTrees(input: List[IndexedSeq[Boolean]], across: Int, down: Int): IO[(Int, String)] = IO {
    input.zipWithIndex
      .collect { case (row, i) if i % down == 0 => row }
      .foldLeft(0 -> 0) { case ((count, idx), row) =>
        if (row(idx % row.size))
          (count + 1) -> (idx + across)
        else
          (count) -> (idx + across)
      }
  }.map(_._1 -> s"r${across}d${down}")

}
