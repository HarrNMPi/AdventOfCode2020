import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

object Day6 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day6.txt")) {
    source => source.getLines().mkString(" ")
  }

  val questionAnswers = input match {
    case Success(data) =>
      val answers = data.split("  ")
      answers
  }

  val part1AnswerFormat = {
    for (answer <- questionAnswers) yield {
      answer.replaceAll("\\s", "")
    }
  }

  val part2AnswerFormat = {
    for (answer <- questionAnswers) yield {
      answer.split(" ")
    }
  }

  @tailrec
  def numberOfTrue(answers: String, uniqueList: String = "", indexNum: Int = 0): Int = {
    if (indexNum == answers.length) uniqueList.length
    else if (!uniqueList.contains(answers.charAt(indexNum))) numberOfTrue(answers, uniqueList + answers.charAt(indexNum), indexNum + 1)
    else numberOfTrue(answers, uniqueList, indexNum + 1)
  }

  @tailrec
  def numberOfTrue2(answer: String, group: Array[String], uniqueList: String = "", indexNum: Int = 0): Int = {
    if (group.length == 1) answer.length
    else if (indexNum == answer.length) uniqueList.length
    else if (!uniqueList.contains(answer.charAt(indexNum)) && group.mkString("").count(_ == answer.charAt(indexNum)) == group.length) numberOfTrue2(answer, group, uniqueList + answer.charAt(indexNum), indexNum + 1)
    else if (uniqueList.contains(answer.charAt(indexNum))) numberOfTrue2(answer, group, uniqueList, indexNum + 1)
    else if (!uniqueList.contains(answer.charAt(indexNum)) && group.mkString("").count(_ == answer.charAt(indexNum)) != group.length) numberOfTrue2(answer, group, uniqueList, indexNum + 1)
    else -1
  }

  def attempt2: Int = {
    val totalArray = for (group <- part2AnswerFormat) yield {
      numberOfTrue2(group(0), group)
    }
    totalArray.sum
  }

  def totalTrue: Int = {
    val totalArray = for (answers <- part1AnswerFormat) yield {
      numberOfTrue(answers)
    }
    totalArray.sum
  }

  println(totalTrue)
  println(attempt2)

}
