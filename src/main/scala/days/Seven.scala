package days

import cats.effect.{ContextShift, IO}
import main.Main.Logger

case class Rule(colour: String, contents: List[(Int, String)])

class Seven(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("seven")
  override type IN1 = Rule
  override type IN2 = Rule

  override def calculatePartOne(input: List[Rule]): IO[String] = IO {
    val asMap = input
      .map { case Rule(colour, contents) => colour -> contents}
      .toMap

    def hasGold(colour: String): Boolean = asMap.get(colour) match {
      case Some(Nil) | None => false
      case Some(l) => l.exists(_._2 == "shinygold") || l.exists(p => hasGold(p._2))
    }
    input.count(r => hasGold(r.colour)).toString
  }

  override def calculatePartTwo(input: List[Rule]): IO[String] = IO {
    val asMap = input
      .map { case Rule(colour, contents) => colour -> contents}
      .toMap

    def count(colour: String): Int = asMap.get(colour) match {
      case None => 0
      case Some(Nil) => 1
      case Some(contents) => 1 + contents.map { case (c, subcontents) => c * count(subcontents) }.sum
    }
    (count("shinygold") - 1).toString
  }

  override def parsePartOne: String => IO[Rule] = s => {
    def parseRules(words: List[String]): List[(Int, String)] =
      words
        .map(_.replace(".", "").replace(",", "")) match {
      case "no" :: "other" :: "bags" :: Nil | Nil => Nil
      case int :: col1 :: col2 :: bag :: rest => (int.toInt -> (col1 + col2)) :: parseRules(rest)
      case other => throw new IllegalArgumentException(s"Unparsable: $other")
    }

      s.split(" ").toList match {
      case col1 :: col2 :: "bags" :: "contain" :: rules => IO(Rule(col1 + col2, parseRules(rules)))
      case other => log.errorAndThrow(s"Unparsable: $other")
    }
  }

  override def parsePartTwo: String => IO[Rule] = parsePartOne
}


