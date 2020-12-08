package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import main.Main.Logger

import scala.annotation.tailrec
import scala.util.Try

sealed trait Command
object Command {
  case class NOP(unused: Int) extends Command
  case class JMP(int: Int) extends Command
  case class ACC(int: Int) extends Command

  def parse(s: String): IO[Command] = s.split(' ') match {
    case Array("nop", i) => IO.fromTry(Try(NOP(i.toInt)))
    case Array("jmp", i) => IO.fromTry(Try(JMP(i.toInt)))
    case Array("acc", i) => IO.fromTry(Try(ACC(i.toInt)))
    case other => IO.raiseError(new IllegalArgumentException(s"What is $s? (${other.mkString("Array(", ", ", ")")})"))
  }

}
class Eight(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("eight")
  override type IN1 = Command
  override type IN2 = Command

  override def calculatePartOne(input: List[Command]): IO[String] = run(input).map(_.toString)

  override def calculatePartTwo(input: List[Command]): IO[String] = {
    val allPossible: List[List[Command]] = input.zipWithIndex.map {
      case (Command.ACC(_), _) => input
      case (Command.JMP(int), idx) => input.updated(idx, Command.NOP(int))
      case (Command.NOP(int), idx) =>  input.updated(idx, Command.JMP(int))
    }
    allPossible.map(run).sequence.flatMap(_.zipWithIndex.collect {
      case (ProgramResult.Success(acc), i) => s"Replacing $i gave $acc"
    } match {
      case head :: Nil => IO(head)
      case head :: tail => log.errorAndThrow(s"Many answers! ${head :: tail}")
      case Nil => log.errorAndThrow("No answers!")
    })
  }

  override def parsePartOne: String => IO[Command] = Command.parse

  override def parsePartTwo: String => IO[Command] = parsePartOne

  /**
   * Returns either a Left(
   */
  def run(program: List[Command]): IO[ProgramResult] = {
    val indexed = program.toIndexedSeq

    @tailrec
    def go(idx: Int, acc: Int, visited: List[Int]): ProgramResult = {
      if (idx == indexed.size)
        ProgramResult.Success(acc)
      else if (visited.toSet.contains(idx))
        ProgramResult.InfiniteLoop(acc, idx :: visited)
      else
        indexed(idx) match {
          case Command.NOP(_) => go(idx + 1, acc, idx :: visited)
          case Command.JMP(int) => go(idx + int, acc, idx :: visited)
          case Command.ACC(int) => go(idx + 1, acc + int, idx :: visited)
        }
    }

    IO(go(0, 0, Nil))
  }

  sealed trait ProgramResult
  object ProgramResult {
    case class Success(acc: Int) extends ProgramResult
    case class InfiniteLoop(acc: Int, visited: List[Int]) extends ProgramResult
  }
}


