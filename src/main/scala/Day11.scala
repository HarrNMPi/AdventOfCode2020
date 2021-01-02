import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

class Seat(empty: Boolean = false, occupied: Boolean = false, floor: Boolean = false) {
  def isEmpty: Boolean = {
    if (empty) true else false
  }

  def isOccupied: Boolean = {
    if (occupied) true else false
  }

  def isFloor: Boolean = {
    if (floor) true else false
  }
}

object Day11 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day11.txt")) {
    source => source.getLines().mkString("\n").split("\n")
  }

  val data = input match {
    case Success(data) => for (x <- data) yield {
      x.split("")
    }
  }

  val seatFormat: Array[Array[Seat]] = {
    for (row <- data) yield {
      for (seat <- row) yield {
        seat match {
          case "L" => new Seat(empty = true)
          case "." => new Seat(floor = true)
          case "#" => new Seat(occupied = true)
        }
      }
    }
  }

  def mapSeats(seatFormat: Array[Array[Seat]]): Array[Array[String]] = {
    for (row <- seatFormat) yield {
      for (seat <- row) yield {
        seat match {
          case _ if seat.isEmpty => "L"
          case _ if seat.isOccupied => "#"
          case _ if seat.isFloor => "."
        }
      }
    }
  }

  def adjacentSeats(firstIndex: Int, secondIndex: Int): Array[(Int, Int)] = {
    if (firstIndex == 0 && secondIndex == 0) {
      Array((firstIndex, secondIndex + 1), (firstIndex + 1, secondIndex), (firstIndex + 1, secondIndex + 1))
    }
    else if (firstIndex == 0 && secondIndex == seatFormat(firstIndex).length - 1) {
      Array((firstIndex, secondIndex - 1), (firstIndex + 1, secondIndex), (firstIndex + 1, secondIndex - 1))
    }
    else if (firstIndex == 0) {
      Array((firstIndex, secondIndex - 1), (firstIndex, secondIndex + 1), (firstIndex + 1, secondIndex), (firstIndex + 1, secondIndex - 1), (firstIndex + 1, secondIndex + 1))
    }
    else if (firstIndex == seatFormat.length - 1 && secondIndex == 0) {
      Array((firstIndex, secondIndex + 1), (firstIndex - 1, secondIndex), (firstIndex - 1, secondIndex + 1))
    }
    else if (firstIndex == seatFormat.length - 1 && secondIndex == seatFormat(firstIndex).length - 1) {
      Array((firstIndex, secondIndex - 1), (firstIndex - 1, secondIndex), (firstIndex - 1, secondIndex - 1))
    }
    else if (firstIndex == seatFormat.length - 1) {
      Array((firstIndex, secondIndex - 1), (firstIndex, secondIndex + 1), (firstIndex - 1, secondIndex), (firstIndex - 1, secondIndex - 1), (firstIndex - 1, secondIndex + 1))
    }
    else if (secondIndex == 0) {
      Array((firstIndex, secondIndex + 1), (firstIndex + 1, secondIndex), (firstIndex - 1, secondIndex), (firstIndex - 1, secondIndex + 1), (firstIndex + 1, secondIndex + 1))
    }
    else if (secondIndex == seatFormat(firstIndex).length - 1) {
      Array((firstIndex, secondIndex - 1), (firstIndex + 1, secondIndex), (firstIndex - 1, secondIndex), (firstIndex - 1, secondIndex - 1), (firstIndex + 1, secondIndex - 1))
    }
    else Array((firstIndex, secondIndex - 1), (firstIndex, secondIndex + 1), (firstIndex + 1, secondIndex), (firstIndex - 1, secondIndex), (firstIndex - 1, secondIndex - 1), (firstIndex - 1, secondIndex + 1), (firstIndex + 1, secondIndex - 1), (firstIndex + 1, secondIndex + 1))
  }

  val positions = Array("up", "down", "right", "left", "topRightDiagonal", "topLeftDiagonal", "bottomRightDiagonal", "bottomLeftDiagonal")

  @tailrec
  def nearestAdjacentSeats(firstIndex: Int, secondIndex: Int, position: Int, x: Int, adjacentSeats: List[(Int, Int)]): List[(Int, Int)] = {
    if (position == 8) adjacentSeats
    else {
      if (position == 0) {
        if (firstIndex - x < 0) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex - x)(secondIndex).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex - x, secondIndex)))
      }
      else if (position == 1) {
        if (firstIndex + x >= 93) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex + x)(secondIndex).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex + x, secondIndex)))
      }
      else if (position == 2) {
        if (secondIndex + x >= 97) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex)(secondIndex + x).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex, secondIndex + x)))
      }
      else if (position == 3) {
        if (secondIndex - x < 0) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex)(secondIndex - x).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex, secondIndex - x)))
      }
      else if (position == 4) {
        if (firstIndex - x < 0 || secondIndex + x >= 97) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex - x)(secondIndex + x).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex - x, secondIndex + x)))
      }
      else if (position == 5) {
        if (firstIndex - x < 0 || secondIndex - x < 0) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex - x)(secondIndex - x).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex - x, secondIndex - x)))
      }
      else if (position == 6) {
        if (firstIndex + x >= 93 || secondIndex + x >= 97) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex + x)(secondIndex + x).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex + x, secondIndex + x)))
      }
      else if (position == 7) {
        if (firstIndex + x >= 93 || secondIndex - x < 0) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats)
        else if (seatFormat(firstIndex + x)(secondIndex - x).isFloor) nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position, x + 1, adjacentSeats = adjacentSeats)
        else nearestAdjacentSeats(firstIndex = firstIndex, secondIndex = secondIndex, position = position + 1, x = 1, adjacentSeats = adjacentSeats ::: List((firstIndex + x, secondIndex - x)))
      }
      else throw new Exception("uh oh")
    }
  }

  def numOfOccupiedSeats(firstIndex: Int, secondIndex: Int, seatLayout: Array[Array[Seat]]): Int = {
    val seats = for (seat <- adjacentSeats(firstIndex, secondIndex) if seatLayout(seat._1)(seat._2).isOccupied) yield {
      seat
    }
    seats.length
  }

  def numOfOccupiedSeatsPartTwo(firstIndex: Int, secondIndex: Int, seatLayout: Array[Array[Seat]]): Int = {
    val seats = for (seat <- nearestAdjacentSeats(firstIndex, secondIndex, 0, 1, List()) if seatLayout(seat._1)(seat._2).isOccupied) yield {
      seat
    }
    seats.length
  }

  def canSeatBeOccupiedPartOne(firstIndex: Int, secondIndex: Int, seatLayout: Array[Array[Seat]]): Boolean = {
    if (numOfOccupiedSeats(firstIndex, secondIndex, seatLayout) == 0) true
    else false
  }

  def willSeatBecomeEmptyPart1(firstIndex: Int, secondIndex: Int, seatLayout: Array[Array[Seat]]): Boolean = {
    if (numOfOccupiedSeats(firstIndex, secondIndex, seatLayout) >= 4) true
    else false
  }

  def canSeatBeOccupiedPartTwo(firstIndex: Int, secondIndex: Int, seatLayout: Array[Array[Seat]]): Boolean = {
    if (numOfOccupiedSeatsPartTwo(firstIndex, secondIndex, seatLayout) == 0) true
    else false
  }

  def willSeatBecomeEmptyPartTwo(firstIndex: Int, secondIndex: Int, seatLayout: Array[Array[Seat]]): Boolean = {
    if (numOfOccupiedSeatsPartTwo(firstIndex, secondIndex, seatLayout) >= 5) true
    else false
  }

  def roundOfRules(seats: Array[Array[Seat]]): Array[Array[Seat]] = {
    for (row <- seats) yield {
      for (seat <- row) yield {
        if (seat.isEmpty && canSeatBeOccupiedPartOne(seats.indexOf(row), row.indexOf(seat), seats)) {
          val newSeat = new Seat(occupied = true)
          newSeat
        }
        else if (seat.isOccupied && willSeatBecomeEmptyPart1(seats.indexOf(row), row.indexOf(seat), seats)) {
          val newSeat = new Seat(empty = true)
          newSeat
        }
        else seat
      }
    }
  }

  def partTwoRules(seats: Array[Array[Seat]]): Array[Array[Seat]] = {
    for (row <- seats) yield {
      for (seat <- row) yield {
        if (seat.isEmpty && canSeatBeOccupiedPartTwo(seats.indexOf(row), row.indexOf(seat), seats)) {
          val newSeat = new Seat(occupied = true)
          newSeat
        }
        else if (seat.isOccupied && willSeatBecomeEmptyPartTwo(seats.indexOf(row), row.indexOf(seat), seats)) {
          val newSeat = new Seat(empty = true)
          newSeat
        }
        else seat
      }
    }
  }

  def areSeatLayoutsSame(firstLayout: Array[Array[Seat]], secondLayout: Array[Array[Seat]]): Boolean = {
    val first: Array[Array[String]] = mapSeats(firstLayout)
    val second: Array[Array[String]] = mapSeats(secondLayout)
    if (first.flatten[String].sameElements(second.flatten[String])) true
    else false
  }

  @tailrec
  def partOne(previousLayout: Array[Array[Seat]]): Int = {
    val newLayout = roundOfRules(previousLayout)
    println(mapSeats(newLayout)(0).mkString("Array(", ", ", ")"))
    println(mapSeats(newLayout)(1).mkString("Array(", ", ", ")"))
    if (areSeatLayoutsSame(previousLayout, newLayout)) {
      val x = for (row <- newLayout) yield {
        row.count(x => x.isOccupied)
      }
      x.sum
    }
    else {
      partOne(previousLayout = newLayout)
    }
  }

  @tailrec
  def partTwo(previousLayout: Array[Array[Seat]]): Int = {
    val newLayout = partTwoRules(previousLayout)
    println(mapSeats(newLayout)(0).mkString("Array(", ", ", ")"))
    println(mapSeats(newLayout)(1).mkString("Array(", ", ", ")"))
    if (areSeatLayoutsSame(previousLayout, newLayout)) {
      val x = for (row <- newLayout) yield {
        row.count(x => x.isOccupied)
      }
      x.sum
    }
    else {
      partTwo(previousLayout = newLayout)
    }
  }

  //println(partOne(seatFormat))
  println(partTwo(seatFormat))
}

