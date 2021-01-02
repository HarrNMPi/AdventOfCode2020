import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

object Day9 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day9.txt")) {
    source => source.getLines().mkString("\n").split("\n")
  }

  val data = input match {
    case Success(data) => data
  }

  @tailrec
  def task(input: Array[String], index: Int, queryIndex: Int): String = {
    if (index == input.length) throw new Exception(s"I'm at index 1000  $index $queryIndex")
    else {
      val diff = (BigInt(input(index)) - BigInt(input(queryIndex))).toString
      if (queryIndex == index) {
        task(input = input, index = index + 1, queryIndex = index - 25)
      }
      else if (queryIndex == index - 1 && !input.slice(index - 25, index).contains(diff)) {
        input(index)
      }
      else if (input.slice(index - 25, index).contains(diff)) {
        task(input = input, index = index + 1, queryIndex = index - 25)
      }
      else if (!input.slice(index - 25, index).contains(diff)) task(input = input, index = index, queryIndex = queryIndex + 1)
      else input(index)
    }
  }

  @tailrec
  def taskTwo(input: Array[String], indexStart: Int, currentIndex: Int, numList: List[BigInt] = List(), answer: BigInt): List[BigInt] = {
    if (numList.sum + BigInt(input(currentIndex)) == answer) numList ::: List(BigInt(input(currentIndex)))
    else if (indexStart == 593) throw new Exception(s"reached index of answer: $indexStart")
    else if (numList.sum + BigInt(input(currentIndex)) > answer) taskTwo(input = input, indexStart = indexStart + 1, currentIndex = indexStart + 1, numList = List(), answer = answer)
    else if (numList.sum + BigInt(input(currentIndex)) < answer) taskTwo(input = input, indexStart = indexStart, currentIndex = currentIndex + 1, numList = numList ::: List(BigInt(input(currentIndex))), answer = answer)
    else throw new Exception(s"uh oh, $indexStart, $currentIndex")
  }

  val answerPart1 = task(data, index = 25, queryIndex = 25)
  val answerPart2 = taskTwo(data, indexStart = 0, currentIndex = 0, answer = BigInt(answerPart1))
  println(answerPart2.min + answerPart2.max)

}

