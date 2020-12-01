import cats.effect.IO
import java.io._
import scala.jdk.CollectionConverters._
import cats.effect._

trait Day {
  def one(implicit cs: ContextShift[IO]): IO[Unit]
  def two(implicit cs: ContextShift[IO]): IO[Unit]

  def readLinesFromFile(file: File)(implicit cs: ContextShift[IO]): IO[List[String]] = {
    val blocker = Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.Implicits.global)

    def readAllLines(bufferedReader: BufferedReader)(implicit cs: ContextShift[IO]): IO[List[String]] =
      blocker.delay[IO, List[String]](bufferedReader.lines().iterator().asScala.toList)

    def reader(file: File)(implicit cs: ContextShift[IO]): Resource[IO, BufferedReader] =
      Resource.fromAutoCloseableBlocking(blocker)(IO(new BufferedReader(new FileReader(file))))

    reader(file).use(br => readAllLines(br))
  }
}
