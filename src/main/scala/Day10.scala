import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

object Day10 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day10.txt")) {
    source => source.getLines().mkString("\n").split("\n")
  }

  val data = input match {
    case Success(data) =>
      for (num <- data) yield {
        num.toInt
      }
  }

  @tailrec
  def taskOne(num: Int = 0, num1VoltJumps: Int = 0, num3VoltJumps: Int = 0): Int = {
    if (num == data.max) num1VoltJumps * (num3VoltJumps + 1)
    else if (data.contains(num + 1)) taskOne(num = num + 1, num1VoltJumps = num1VoltJumps + 1, num3VoltJumps = num3VoltJumps)
    else if (data.contains(num + 2)) taskOne(num = num + 2, num1VoltJumps = num1VoltJumps, num3VoltJumps = num3VoltJumps)
    else if (data.contains(num + 3)) taskOne(num = num + 3, num1VoltJumps = num1VoltJumps, num3VoltJumps = num3VoltJumps + 1)
    else throw new Exception("Something went wrong")
  }

  @tailrec
  def partTwo(data: Array[Int] = data, currentIndex: Int, numConsecutive: Int, answer: BigInt): BigInt = {
    val sortedNum = data.sorted.prepended(0)
    if (currentIndex == data.length) {
      println("Finished: " + currentIndex + " " + numConsecutive + " " + sortedNum(currentIndex))
      answer
    }
    else if (sortedNum(currentIndex + 1) == sortedNum(currentIndex) + 1) partTwo(data = data, currentIndex = currentIndex + 2, numConsecutive = numConsecutive + 1, answer = answer)
    else if (sortedNum(currentIndex + 1) != sortedNum(currentIndex) + 1 && numConsecutive == 1) {
      println("else part 1: " + numConsecutive + "  " + currentIndex + "  " + sortedNum(currentIndex) + " " + answer)
      partTwo(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, answer = answer)
    }
    else if (sortedNum(currentIndex + 1) != sortedNum(currentIndex) + 1 && numConsecutive == 3) {
      println("else part 2: " + numConsecutive + "  " + currentIndex + "  " + sortedNum(currentIndex) + " " + answer)
      partTwo(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, answer = answer * 4)
    }
    else if (sortedNum(currentIndex + 1) != sortedNum(currentIndex) + 1 && numConsecutive == 2) {
      println("else part 3: " + numConsecutive + "  " + currentIndex + "  " + sortedNum(currentIndex) + " " + answer)
      partTwo(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, answer = answer * 2)
    }
    else if (sortedNum(currentIndex + 1) != sortedNum(currentIndex) + 1 && numConsecutive == 4) {
      println("else part 4: " + numConsecutive + "  " + currentIndex + "  " + sortedNum(currentIndex))
      partTwo(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, answer = answer * 6)
    }
    else if (numConsecutive == 0) {
      println("num consecutive = 0")
      partTwo(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, answer = answer)
    }
    else throw new Exception("uh oh" + numConsecutive + " " + sortedNum(currentIndex))
  }

  @tailrec
  def partTwoTake666(data: Array[Int] = data, currentIndex: Int, numConsecutive: Int, num2s: Int, num3s: Int, num4s: Int, num5s: Int): Double = {
    if (currentIndex >= data.length - 1) {
      if (numConsecutive == 0) scala.math.pow(13, num5s) * scala.math.pow(7, num4s) * scala.math.pow(4, num3s) * scala.math.pow(2, num2s)
      else if (numConsecutive == 1) scala.math.pow(13, num5s) * scala.math.pow(7, num4s) * scala.math.pow(4, num3s) * scala.math.pow(2, num2s + 1)
      else if (numConsecutive == 2) scala.math.pow(13, num5s) * scala.math.pow(7, num4s) * scala.math.pow(4, num3s + 1) * scala.math.pow(2, num2s)
      else if (numConsecutive == 3) scala.math.pow(13, num5s) * scala.math.pow(7, num4s + 1) * scala.math.pow(4, num3s) * scala.math.pow(2, num2s)
      else if (numConsecutive == 4) scala.math.pow(13, num5s + 1) * scala.math.pow(7, num4s) * scala.math.pow(4, num3s) * scala.math.pow(2, num2s)
      else throw new Exception("numConsecutive is a non expected number at the end")
    }
    else if (data(currentIndex + 1) == data(currentIndex) + 1) partTwoTake666(data = data, currentIndex = currentIndex + 1, numConsecutive = numConsecutive + 1, num2s = num2s, num3s = num3s, num4s = num4s, num5s = num5s)
    else if (data(currentIndex + 1) != data(currentIndex) + 1 && numConsecutive == 0) {
      println("else part 1: " + numConsecutive + "  " + currentIndex + "  " + data(currentIndex))
      partTwoTake666(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, num2s = num2s, num3s = num3s, num4s = num4s, num5s = num5s)
    }
    else if (data(currentIndex + 1) != data(currentIndex) + 1 && numConsecutive == 2) {
      println("else part 2: " + numConsecutive + "  " + currentIndex + "  " + data(currentIndex))
      partTwoTake666(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, num2s = num2s, num3s = num3s + 1, num4s = num4s, num5s = num5s)
    }
    else if (data(currentIndex + 1) != data(currentIndex) + 1 && numConsecutive == 1) {
      println("else part 3: " + numConsecutive + "  " + currentIndex + "  " + data(currentIndex))
      partTwoTake666(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, num2s = num2s + 1, num3s = num3s, num4s = num4s, num5s = num5s)
    }
    else if (data(currentIndex + 1) != data(currentIndex) + 1 && numConsecutive == 3) {
      println("else part 4: " + numConsecutive + "  " + currentIndex + "  " + data(currentIndex))
      partTwoTake666(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, num2s = num2s, num3s = num3s, num4s = num4s + 1, num5s = num5s)
    }
    else if (data(currentIndex + 1) != data(currentIndex) + 1 && numConsecutive == 4) {
      println("else part 5: " + numConsecutive + "  " + currentIndex + "  " + data(currentIndex))
      partTwoTake666(data = data, currentIndex = currentIndex + 2, numConsecutive = 0, num2s = num2s, num3s = num3s, num4s = num4s, num5s = num5s + 1)
    }
    else throw new Exception("uh oh" + numConsecutive + " " + data(currentIndex))
  }

  //println(taskOne())
  val orderedData: Array[Int] = data.sorted.prepended(0)

  //println(partTwo(data, 0, 0, 1))

  val testArray = Array(0, 28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4,
    2, 34, 10, 3).sorted

  val secondTestArray = Array(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4).sorted.prepended(0)

  //println(partTwoTake666(secondTestArray, 1, 0, 0,0,0, 0))
  println(partTwoTake666(testArray, 0, 0, 0, 0, 0, 0))
  //println(partTwoTake666(orderedData, 0, 0, 0, 0, 0, 0))

}

//247390116249600
//65082297369600
//56693912375296
//10413167579136
//14173478093824
//48884036690944
//3962711310336
//452984832
//1114512556032
//635492476982272
