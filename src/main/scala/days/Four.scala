package days

import cats.effect.{ContextShift, IO}
import cats.implicits._
import days.Four.Passport
import main.Main.Logger

class Four(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("four")
  override type IN1 = List[(String, String)]
  override type IN2 = List[(String, String)]

  override def calculatePartOne(input: List[List[(String, String)]]): IO[String] = IO {
    input.foldRight(List.empty[List[(String, String)]] -> List.empty[(String, String)]) { case (line, (finished, current)) =>
      line match {
        case Nil => (current :: finished) -> Nil
        case more => finished -> (current ::: more)

      }
    }
  }
    .map { case (passports, last) => last :: passports }
    .map(_.map(Passport.fromPairsV1(_: _*)))
    .map(_.count(_.isRight).toString)

  override def calculatePartTwo(input: List[List[(String, String)]]): IO[String] = IO {
    input.foldRight(List.empty[List[(String, String)]] -> List.empty[(String, String)]) { case (line, (finished, current)) =>
      line match {
        case Nil => (current :: finished) -> Nil
        case more => finished -> (current ::: more)

      }
    }
  }
    .map { case (passports, last) => last :: passports }
    .map(_.map(Passport.fromPairsV2(_: _*)))
    .map(_.count(_.isRight).toString)

  override def parsePartOne: String => IO[List[(String, String)]] =
    line => IO(line
      .split(" ")
      .map(pair => pair.splitAt(pair.indexOf(":")))
      .collect{ case (a,b) if a.nonEmpty && b.nonEmpty => a -> b.drop(1) }
      .toList
    )

  override def parsePartTwo: String => IO[List[(String, String)]] = parsePartOne
}

object Four {

  case class Passport(byr: Int, iyr: Int, eyr: Int, hgt: String, hcl: String, ecl: String, pid: String, cid: Option[String])

  object Passport {
    private val validEyeColours = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    private val af09 = Set('a','b','c','d','e','f', '0','1','2','3','4','5','6','7','8','9')

    private def validHeight(str: String): Boolean =
      str.length > 2 && {
        val (number, ending) = str.splitAt(str.length - 2)
        number.toIntOption.exists(i => if (ending == "cm") i >= 150 && i <= 193 else if (ending == "in") i >= 59 && i <= 76 else false)
      }

    def fromPairsV1(pairs: (String, String)*): Either[String, Passport] = {
      val asMap = pairs.toMap
      for {
        byr <- asMap.get("byr").flatMap(_.toIntOption).toRight("byr missing or invalid")
        iyr <- asMap.get("iyr").flatMap(_.toIntOption).toRight("iyr missing or invalid")
        eyr <- asMap.get("eyr").flatMap(_.toIntOption).toRight("eyr missing or invalid")
        hgt <- asMap.get("hgt").toRight("hgt missing")
        hcl <- asMap.get("hcl").toRight("hcl missing")
        ecl <- asMap.get("ecl").toRight("ecl missing")
        pid <- asMap.get("pid").toRight("pid missing")
        cid <- Right(asMap.get("cid"))
      } yield Passport(
        byr,
        iyr,
        eyr,
        hgt,
        hcl,
        ecl,
        pid,
        cid
      )
    }

    def fromPairsV2(pairs: (String, String)*): Either[String, Passport] = {
      val asMap = pairs.toMap
      for {
        byr <- asMap.get("byr").flatMap(_.toIntOption).filter(y => y >= 1920 && y <= 2002).toRight("byr missing or invalid")
        iyr <- asMap.get("iyr").flatMap(_.toIntOption).filter(y => y >= 2010 && y <= 2020).toRight("iyr missing or invalid")
        eyr <- asMap.get("eyr").flatMap(_.toIntOption).filter(y => y >= 2020 && y <= 2030).toRight("eyr missing or invalid")
        hgt <- asMap.get("hgt").filter(validHeight).toRight("hgt missing or invalid")
        hcl <- asMap.get("hcl").map(_.splitAt(1)).collect { case ("#", y) if y.length == 6 && y.forall(af09) => s"#$y"}.toRight("hcl missing or invalid")
        ecl <- asMap.get("ecl").filter(validEyeColours).toRight("ecl missing or invalid")
        pid <- asMap.get("pid").filter(s => s.length == 9 && s.forall(_.isDigit)).toRight("pid missing or invalid")
        cid <- Right(asMap.get("cid"))
      } yield Passport(
        byr,
        iyr,
        eyr,
        hgt,
        hcl,
        ecl,
        pid,
        cid
      )
    }
  }

}
