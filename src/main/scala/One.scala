import java.io.File

import cats.effect.{ContextShift, IO}

object One extends Day {
  override def one(implicit cs: ContextShift[IO]): IO[Unit] =
    readLinesFromFile(new File("C:\\dev\\AdventOfCode2020\\src\\main\\resources\\one.txt")).flatMap { lines =>
      val (big, small) = lines
        .map(_.toInt)
        .partition(_ > 1010) //Could not bother with this at all and just dot product the whole thing but let's at least be a LITTLE bit efficient

      val res = for {
        b <- big
        s <- small
        if b + s == 2020
      } yield b * s

      res match {
        case head :: Nil => IO.pure(head)
        case _ :: _ => IO.raiseError(new RuntimeException("More than one answer!"))
        case Nil =>IO.raiseError(new RuntimeException("No answer!"))
      }
    }
      .flatMap(i => IO(println(s"Answer to Day 1 Part 1 is $i")))

  override def two(implicit cs: ContextShift[IO]): IO[Unit] =
    readLinesFromFile(new File("C:\\dev\\AdventOfCode2020\\src\\main\\resources\\one.txt")).flatMap { lines =>
      val all = lines.map(_.toInt)

      //OK sod efficiency
      val res = (for {
        a <- all
        b <- all
        c <- all
        if a + b + c == 2020
      } yield a * b * c).distinct

      res match {
        case head :: Nil => IO.pure(head)
        case _ :: _ => IO.raiseError(new RuntimeException("More than one answer!"))
        case Nil =>IO.raiseError(new RuntimeException("No answer!"))
      }
    }
      .flatMap(i => IO(println(s"Answer to Day 2 Part 2 is $i")))
}