import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

object Day12 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day12.txt")) {
    source => source.getLines().mkString("\n").split("\n")
  }

  val directions = input match {
    case Success(data) => data
  }

  @tailrec
  def partOne(currentDirection: String, x: Int = 0, northSouth: Int = 0, eastWest: Int = 0): Int = {
    if (x == directions.length) northSouth.abs + eastWest.abs
    else {
      if (directions(x).charAt(0) == 'N') {
        val distance = directions(x).split("N")(1).toInt
        partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth + distance, eastWest = eastWest)
      }
      else if (directions(x).charAt(0) == 'S') {
        val distance = directions(x).split("S")(1).toInt
        partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth - distance, eastWest = eastWest)
      }
      else if (directions(x).charAt(0) == 'E') {
        val distance = directions(x).split("E")(1).toInt
        partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth, eastWest = eastWest + distance)
      }
      else if (directions(x).charAt(0) == 'W') {
        val distance = directions(x).split("W")(1).toInt
        partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth, eastWest = eastWest - distance)
      }
      else if (directions(x).charAt(0) == 'L') {
        val degrees = directions(x).split("L")(1).toInt
        val newDirection = currentDirection match {
          case "North" if degrees == 90 => "West"
          case "East" if degrees == 90 => "North"
          case "South" if degrees == 90 => "East"
          case "West" if degrees == 90 => "South"
          case "North" if degrees == 180 => "South"
          case "East" if degrees == 180 => "West"
          case "South" if degrees == 180 => "North"
          case "West" if degrees == 180 => "East"
          case "North" if degrees == 270 => "East"
          case "East" if degrees == 270 => "South"
          case "South" if degrees == 270 => "West"
          case "West" if degrees == 270 => "North"
        }
        partOne(currentDirection = newDirection, x = x + 1, northSouth = northSouth, eastWest = eastWest)
      }
      else if (directions(x).charAt(0) == 'R') {
        val degrees = directions(x).split("R")(1).toInt
        val newDirection = currentDirection match {
          case "North" if degrees == 90 => "East"
          case "East" if degrees == 90 => "South"
          case "South" if degrees == 90 => "West"
          case "West" if degrees == 90 => "North"
          case "North" if degrees == 180 => "South"
          case "East" if degrees == 180 => "West"
          case "South" if degrees == 180 => "North"
          case "West" if degrees == 180 => "East"
          case "North" if degrees == 270 => "West"
          case "East" if degrees == 270 => "North"
          case "South" if degrees == 270 => "East"
          case "West" if degrees == 270 => "South"
        }
        partOne(currentDirection = newDirection, x = x + 1, northSouth = northSouth, eastWest = eastWest)
      }
      else if (directions(x).charAt(0) == 'F') {
        val distance = directions(x).split("F")(1).toInt
        if (currentDirection == "North") partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth - distance, eastWest = eastWest)
        else if (currentDirection == "East") partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth, eastWest = eastWest + distance)
        else if (currentDirection == "South") partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth + distance, eastWest = eastWest)
        else partOne(currentDirection = currentDirection, x = x + 1, northSouth = northSouth, eastWest = eastWest - distance)
      }
      else throw new Exception("Uh oh")
    }
  }

  def partTwo(x: Int = 0, northSouth: Int = 0, eastWest: Int = 0, wayPoint: List[(Int, Int)]): Int = {
    if (x == directions.length) northSouth.abs + eastWest.abs
    else {
      if (directions(x).charAt(0) == 'N') {
        val distance = directions(x).split("N")(1).toInt
        val currentEast = wayPoint.head._1
        val newNorth = wayPoint.head._2 + distance
        partTwo(x = x + 1, northSouth = northSouth, eastWest = eastWest, wayPoint = List((currentEast, newNorth)))
      }
      else if (directions(x).charAt(0) == 'S') {
        val distance = directions(x).split("S")(1).toInt
        val currentEast = wayPoint.head._1
        val newNorth = wayPoint.head._2 - distance
        partTwo(x = x + 1, northSouth = northSouth, eastWest = eastWest, wayPoint = List((currentEast, newNorth)))
      }
      else if (directions(x).charAt(0) == 'E') {
        val distance = directions(x).split("E")(1).toInt
        val newEast = wayPoint.head._1 + distance
        val currentNorth = wayPoint.head._2
        partTwo(x = x + 1, northSouth = northSouth, eastWest = eastWest, wayPoint = List((newEast, currentNorth)))
      }
      else if (directions(x).charAt(0) == 'W') {
        val distance = directions(x).split("W")(1).toInt
        val newEast = wayPoint.head._1 - distance
        val currentNorth = wayPoint.head._2
        partTwo(x = x + 1, northSouth = northSouth, eastWest = eastWest, wayPoint = List((newEast, currentNorth)))
      }
      else if (directions(x).charAt(0) == 'L') {
        val degrees = directions(x).split("L")(1).toInt
        val newDirection = currentDirection match {
          case "North" if degrees == 90 => "West"
          case "East" if degrees == 90 => "North"
          case "South" if degrees == 90 => "East"
          case "West" if degrees == 90 => "South"
          case "North" if degrees == 180 => "South"
          case "East" if degrees == 180 => "West"
          case "South" if degrees == 180 => "North"
          case "West" if degrees == 180 => "East"
          case "North" if degrees == 270 => "East"
          case "East" if degrees == 270 => "South"
          case "South" if degrees == 270 => "West"
          case "West" if degrees == 270 => "North"
        }
        partTwo(x = x + 1, northSouth = northSouth, eastWest = eastWest)
      }
      else if (directions(x).charAt(0) == 'R') {
        val degrees = directions(x).split("R")(1).toInt
        val newDirection = degrees match {
          case 90 if wayPoint.head._1 > 0 && wayPoint.head._2 > 0 => List((-wayPoint.head._2, wayPoint.head._1))
          case 90 if wayPoint.head._1 < 0 && wayPoint.head._2 > 0 => List((-wayPoint.head._2, -wayPoint.head._1))
          case 90 if wayPoint.head._1 < 0 && wayPoint.head._2 < 0 => List((wayPoint.head._2, -wayPoint.head._1))
          case 90 if wayPoint.head._1 > 0 && wayPoint.head._2 < 0 => List((wayPoint.head._2, wayPoint.head._1))
        }
        partTwo(x = x + 1, northSouth = northSouth, eastWest = eastWest, wayPoint = newDirection)
      }
      else if (directions(x).charAt(0) == 'F') {
        val distance = directions(x).split("F")(1).toInt
        val movementEast = wayPoint(0)._1 * distance
        val movementNorth = wayPoint(0)._2 * distance
        partTwo(x = x + 1, northSouth = northSouth + movementNorth, eastWest = eastWest + movementEast, wayPoint = wayPoint)
      }
      else throw new Exception("Uh oh")
    }
  }


  println(directions.mkString("Array(", ", ", ")"))
  println(partOne("East"))

}
