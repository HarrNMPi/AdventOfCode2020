import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

object Day5 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day5.txt")) {
    source => source.getLines().mkString(" ").split(" ")
  }

  val seats = input match {
    case Success(seatData) => seatData
  }

  @tailrec
  def findSeatRow(seat: String, low: Int = 0, high: Int = 127, indexNum: Int = 0): Int = {
    if (indexNum == seat.length - 4 && seat.charAt(indexNum) == 'F') low
    else if (indexNum == seat.length - 4 && seat.charAt(indexNum) == 'B') high
    else if (seat.charAt(indexNum) == 'F') findSeatRow(seat, low, low + ((high - low - 1) / 2), indexNum + 1)
    else if (seat.charAt(indexNum) == 'B') findSeatRow(seat, low + ((high - low) + 1) / 2, high, indexNum + 1)
    else -1
  }

  @tailrec
  def findSeatColumn(seat: String, low: Int = 0, high: Int = 7, indexNum: Int = 7): Int = {
    if (indexNum == 9 && seat.charAt(indexNum) == 'L') low
    else if (indexNum == 9 && seat.charAt(indexNum) == 'R') high
    else if (seat.charAt(indexNum) == 'L') findSeatColumn(seat, low, low + ((high - low - 1) / 2), indexNum + 1)
    else if (seat.charAt(indexNum) == 'R') findSeatColumn(seat, low + ((high - low) + 1) / 2, high, indexNum + 1)
    else -1
  }

  val seatRows: Array[Int] =
    for (seat <- seats) yield {
      findSeatRow(seat)
    }

  val seatColumns: Array[Int] =
    for(seat <- seats) yield {
      findSeatColumn(seat)
    }

  val seatId: Array[Int] =
    for (seat <- seats) yield {
      (findSeatRow(seat) * 8) + findSeatColumn(seat)
    }

  @tailrec
  def mySeat(seatIds: Array[Int], id: Int = 0): Int = {
    if (!seatIds.contains(id + 1) && seatIds.contains(id + 2) && seatIds.contains(id)) id + 1
    else mySeat(seatIds, id + 1)
  }

  val highestSeatId = seatId.max

  println(highestSeatId)
  println(mySeat(seatId.sorted))

}
