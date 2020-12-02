import cats.implicits._
import cats.effect.IO
import java.io._
import scala.jdk.CollectionConverters._
import cats.effect._

trait Day {
  val name: String
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
      parsed <- lines.map(parsePartOne).sequence
      res <- calculatePartOne(parsed)
    } yield res

  def two(implicit cs: ContextShift[IO]): IO[String] =
    for {
      lines <- readLinesFromFile(partTwoFile)
      parsed <- lines.map(parsePartTwo).sequence
      res <- calculatePartTwo(parsed)
    } yield res

  private def readLinesFromFile(file: File)(implicit cs: ContextShift[IO]): IO[List[String]] = {
    val blocker = Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.Implicits.global)

    def readAllLines(bufferedReader: BufferedReader)(implicit cs: ContextShift[IO]): IO[List[String]] =
      blocker.delay[IO, List[String]](bufferedReader.lines().iterator().asScala.toList)

    def reader(file: File)(implicit cs: ContextShift[IO]): Resource[IO, BufferedReader] =
      Resource.fromAutoCloseableBlocking(blocker)(IO(new BufferedReader(new FileReader(file))))

    reader(file).use(br => readAllLines(br))
  }
}
