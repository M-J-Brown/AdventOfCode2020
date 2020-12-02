import java.io._

import cats.effect.{IO, _}
import cats.implicits._

import scala.jdk.CollectionConverters._

trait Day {
  val name: String

  override def toString: String = name

  type IN1
  type IN2

  def calculatePartOne(input: List[IN1])(implicit cs: ContextShift[IO]): IO[String]

  def calculatePartTwo(input: List[IN2])(implicit cs: ContextShift[IO]): IO[String]

  def parsePartOne: String => IO[IN1]

  def parsePartTwo: String => IO[IN2]

  lazy val partOneFile = new File(s"C:\\dev\\AdventOfCode2020\\src\\main\\resources\\${name}_1.txt")
  lazy val partTwoFile = new File(s"C:\\dev\\AdventOfCode2020\\src\\main\\resources\\${name}_2.txt")

  def one(implicit cs: ContextShift[IO]): IO[String] =
    for {
      lines <- readLinesFromFile(partOneFile)
      parsed <- lines.map(safeParse(_, parsePartOne)).sequence
      res <- calculatePartOne(parsed).attempt.flatMap {
        case Left(err) =>  IO.raiseError(new RuntimeException(s"Error calculating with $parsed for day $name part one: $err"))
        case Right(value) => IO.pure(value)
      }
    } yield res

  def two(implicit cs: ContextShift[IO]): IO[String] =
    for {
      lines <- readLinesFromFile(partTwoFile)
      parsed <- lines.map(safeParse(_, parsePartTwo)).sequence
      res <- calculatePartTwo(parsed).attempt.flatMap {
        case Left(err) =>  IO.raiseError(new RuntimeException(s"Error calculating with $parsed for day $name part two: $err"))
        case Right(value) => IO.pure(value)
      }
    } yield res

  def safeParse[A](string: String, parseFn: String => IO[A]): IO[A] = parseFn(string).attempt.flatMap {
    case Left(err) =>  IO.raiseError(new RuntimeException(s"Error parsing input $string for day $name: $err"))
    case Right(value) => IO.pure(value)
  }

  private def readLinesFromFile(file: File)(implicit cs: ContextShift[IO]): IO[List[String]] = {
    val blocker = Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.Implicits.global)

    def readAllLines(bufferedReader: BufferedReader)(implicit cs: ContextShift[IO]): IO[List[String]] =
      blocker.delay[IO, List[String]](bufferedReader.lines().iterator().asScala.toList)

    def reader(file: File)(implicit cs: ContextShift[IO]): Resource[IO, BufferedReader] =
      Resource.fromAutoCloseableBlocking(blocker)(IO(new BufferedReader(new FileReader(file))))

    reader(file).use(br => readAllLines(br)).attempt.flatMap {
      case Left(err) =>  IO.raiseError(new RuntimeException(s"Error reading file $file for day $name: $err"))
      case Right(value) => IO.pure(value)
    }
  }
}
