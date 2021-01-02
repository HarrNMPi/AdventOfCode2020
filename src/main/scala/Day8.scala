import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

class TaskResponse

case class TaskSuccess(x: Int) extends TaskResponse

case class TaskFailure(x: Int) extends TaskResponse

object Day8 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day8.txt")) {
    source => source.getLines().mkString("\n").split("\n")
  }

  val instructions = input match {
    case Success(data) =>
      for (rules <- data) yield {
        rules.split(" ")
      }
  }

  @tailrec
  def task(instructions: Array[Array[String]], indexCounter: Int = 0, accumulator: Int = 0, indexList: List[Int] = List()): TaskResponse = {
    if (indexCounter == instructions.length) TaskSuccess(accumulator)
    else if (indexList.contains(indexCounter)) {
      TaskFailure(accumulator)
    }
    else {
      instructions(indexCounter)(0) match {
        case "nop" => task(instructions, indexCounter + 1, accumulator = accumulator, indexList = indexList ++ List(indexCounter))
        case "jmp" => task(instructions, indexCounter + instructions(indexCounter)(1).toInt, accumulator = accumulator, indexList = indexList ++ List(indexCounter))
        case "acc" => task(instructions, indexCounter + 1, accumulator = accumulator + instructions(indexCounter)(1).toInt, indexList = indexList ++ List(indexCounter))
      }
    }
  }

  @tailrec
  def getAlljmp(instructions: Array[Array[String]], indexList: List[Int] = List(), indexNum: Int = 0): Array[Int] = {
    if (indexNum == instructions.length) indexList.toArray
    else if (instructions(indexNum)(0).contains("jmp")) getAlljmp(instructions = instructions, indexList = indexList ++ List(indexNum), indexNum = indexNum + 1)
    else getAlljmp(instructions = instructions, indexList = indexList, indexNum = indexNum + 1)
  }

  @tailrec
  def taskTwo(instructions: Array[Array[String]], jmpIndexes: Array[Int], indexNum: Int = 0): Int = {
    if (indexNum == jmpIndexes.length) -1 //would have called similar method but replacing all nop with jmp instead
    else {
      val newInstructions: Array[Array[String]] = for (instruction <- instructions) yield {
        for (x <- instruction) yield {
          if (instructions.indexOf(instruction) == jmpIndexes(indexNum)) x.replace("jmp", "nop")
          else x
        }
      }
      task(newInstructions) match {
        case TaskSuccess(x) => {
          println("Success")
          x
        }
        case TaskFailure(_) => taskTwo(instructions, jmpIndexes = jmpIndexes, indexNum = indexNum + 1)
        case _ => throw new Exception("uh oh")
      }
    }

  }

  println(task(instructions))
  println(taskTwo(instructions, getAlljmp(instructions)))
}
