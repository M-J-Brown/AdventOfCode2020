package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import days.Space.{Floor, Occupied}
import main.Main.Logger

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Try


sealed trait Space {
  override def toString: String = this match {
    case Space.Empty => "L"
    case Space.Occupied => "#"
    case Space.Floor => "."
  }
}

object Space {

  case object Empty extends Space

  case object Occupied extends Space

  case object Floor extends Space

  def fromChar(c: Char): Option[Space] = c match {
    case 'L' => Space.Empty.some
    case '#' => Space.Occupied.some
    case '.' => Space.Floor.some
    case _ => None
  }
}

class Eleven(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  type TwoDimensionalArray[A] = Array[Array[A]]

  override implicit val name: DayName = DayName("eleven")
  override type IN1 = Array[Space]
  override type IN2 = Array[Space]

  override def calculatePartOne(input: List[Array[Space]]): IO[String] = {
    val asArray = input.toArray

      def mutate(input: TwoDimensionalArray[Space]): IO[TwoDimensionalArray[Space]] = IO(
        input.map2DWithIndex { case (cell, x, y) =>
          cell match {
            case Space.Empty if input.pt1Neighbours(x, y).forall(_ != Occupied) => Space.Occupied
            case Space.Occupied if input.pt1Neighbours(x, y).count(_ == Occupied) > 3 => Space.Empty
            case other => other
          }
        }
      )

      def go(input: TwoDimensionalArray[Space], count: Int): IO[(Int, TwoDimensionalArray[Space])] =
        mutate(input).flatMap { next =>
          if (next.toList.map(_.toList) == input.toList.map(_.toList))
            IO.pure(count -> next)
          else
            go(next, count + 1)
        }

      go(asArray, 0)
      //.flatTap { case (_, finished) => log.info(finished.pretty) }
      .map { case (count, finished) => s"After $count rounds, ${finished.count2D(_ == Occupied)} occupied seats" }
  }

  override def calculatePartTwo(input: List[Array[Space]]): IO[String] = {
    val asArray = input.toArray
      def mutate(input: TwoDimensionalArray[Space]): IO[TwoDimensionalArray[Space]] = IO(
        input.map2DWithIndex { case (cell, x, y) =>
          cell match {
            case Space.Empty if input.pt2Neighbours((x,y), _ != Floor).forall(_ != Occupied) => Space.Occupied
            case Space.Occupied if input.pt2Neighbours((x,y), _ != Floor).count(_ == Occupied) > 4 => Space.Empty
            case other => other
          }
        }
      )

      def go(input: TwoDimensionalArray[Space], count: Int): IO[(Int, TwoDimensionalArray[Space])] =
        mutate(input).flatMap { next =>
          if (next.toList.map(_.toList) == input.toList.map(_.toList))
              IO.pure(count -> next)
          else
            go(next, count + 1)
        }

      go(asArray, 0)
      //.flatTap { case (_, finished) => log.info(finished.pretty) }
      .map { case (count, finished) => s"After $count rounds, ${finished.count2D(_ == Occupied)} occupied seats" }
  }

  override def parsePartOne: String => IO[Array[Space]] = s =>
    s.map(Space.fromChar)
      .toList
      .sequence
      .map(_.toArray)
      .liftTo[IO](new IllegalArgumentException(s"Bad char in $s"))

  override def parsePartTwo: String => IO[Array[Space]] = parsePartOne


  implicit class TwoDimensionalOps[A](array: TwoDimensionalArray[A]) {
    def at(tuple: (Int, Int)): Option[A] = at(tuple._1, tuple._2)

    def at(x: Int, y: Int): Option[A] = Try(array(y)(x)).toOption

    def map2DWithIndex[B: ClassTag](f: (A, Int, Int) => B): TwoDimensionalArray[B] =
      array.zipWithIndex.map { case (row, y) => row.zipWithIndex.map { case (cell, x) => f(cell, x, y) } }

    def count2D(f: A => Boolean): Int = array.map(_.count(f)).sum

    def pretty: String = "\n" + array.map(_.mkString("")).mkString("\n")

    def pt1Neighbours(coords: (Int, Int)): List[A] =
      units.map(_ |+| coords).flatMap(at)

    def pt2Neighbours(coords: (Int, Int), valid: A => Boolean): List[A] = {
      @tailrec
      def recurse(unit: (Int, Int), prev: (Int, Int)): Option[A] = at(prev |+| unit) match {
        case Some(value) if valid(value) => Some(value) //found what we're looking for
        case Some(_) => recurse(unit, prev |+| unit)
        case None => None //Hit the edge
      }

      units.flatMap(recurse(_, coords))
    }
  }
  val units: List[(Int, Int)] = {
    val all = List(1,0,-1)
    (for {
      x <- all
      y <- all
    } yield x -> y)
      .filter { case (x, y) => x != 0 || y != 0 }
  }
}