import scala.collection.mutable.ListBuffer

object Day3 extends App {

  val data = "...........#..#.#.###....#.....\n...#..#...........#.#...#......\n#.....#..#........#...#..##....\n..#...##.#.....#.............#.\n#.#..#......#.....#....#.......\n.....#......#..#....#.....#....\n.......#.#..............#......\n.....#...#..........##...#.....\n...#....#.#...#.#........#...#.\n..#.........###.......##...##..\n.#....#...........#........#..#\n..#.............##.............\n..#.##.#....#................#.\n.....##.#.......#....#...#.....\n......#.#....##................\n..#..........###..#..#.#..#....\n....#..............#....##..#.#\n.#.........#.#....#.#.#....#...\n..#.....#......##.#....#.......\n..#.#....#..#.#...##....###....\n...#......##...#........#.#..#.\n.##.#.......##....#............\n...##..#.#............#...#.#..\n.##...##.#..#..................\n..#......##......#......##.....\n.....##...#..#...#.........#...\n.##.#.....#..#..#.##....##....#\n..#.#......#.......##..........\n......................#......##\n##.#...#.................#.#.#.\n......#.#..........#.....##.#..\n#.#......#.....#...........#...\n.....#...#.......#..#..#.#...#.\n...........#......#.#...#......\n....##...##...........#......#.\n.........#.##..................\n......#...#....#......##.##...#\n......#...#.#########......#...\n.......#.#...#.......#..#......\n............#...#...#.###...##.\n...........#..........#...#....\n...#..#.#................#.#..#\n..#....#.....#.............#.#.\n....##......#........#....#....\n........##...............#....#\n........#..#...#..............#\n...#....#.#...#..#...#....#.#.#\n.........#.......#....##.......\n#.##..............#.#........##\n......................###......\n.........#....##..##....#.#.#..\n.#...##.#.#...#....##..#.....#.\n....................#.#......#.\n.#..#.......................#..\n..#..#.............#..#....#...\n...#...#..#...#...#.#..#.##....\n........#....#...#....#........\n.#.....#........#.........#...#\n...#......#..#..#..####..#.....\n#....#..............#.##.......\n.#....#.............##...#.....\n....#...#.##........##......#..\n##....#...#.......#..#........#\n....##........................#\n..................#..#.........\n..#....#........#..#.......#...\n#...#..#....#...##...........#.\n.........#..#..#.#.##..........\n....#.#..#.....#..#.#.#.#..#.##\n##................#.##.....#...\n.#.....###..#..#..#.....#....##\n...#...........#..........####.\n.#.....#....#......#.##..#.#...\n..#...##....#................#.\n........#.......#......#.#.....\n....#.#.#.#....#...#......#..#.\n...........#......#..#.........\n###...##......##.#..#....##....\n##....##.........#..#....###...\n#.#.....#....#......#...#..##..\n#....##.#..............#.##....\n.#........#.#.........#...#....\n......................#......#.\n........#..#..##.....#..#.#....\n..#...###.................#..#.\n...#...#............#..........\n.##.......#..#.........#....#..\n.#..............#....#....##...\n...............##..#.#.......##\n.#.....#....#...#..#.......#..#\n#..#.............#....#......#.\n.....#.#......#.........###..#.\n.#...#.#...............#....#..\n#......#.............#.........\n.#.##.#.####...#..#.##..#.....#\n.....#......#..#...#.......#...\n#........###...#.....#..#.....#\n....#.#.....#...#..........#...\n...#.#.......#.#......#..#.##..\n..#..........#.#..#.......#.#..\n#...#.#..............#...###.#.\n...#..#...#............###.....\n..#..#...#.#............#..#...\n.###.#.....................#..#\n....#....#..#.....##.##........\n#....#....#.#..#.........#.....\n.#.....##....#............##..#\n#....#.#...#...#..#.#......#...\n#.....##.....##.....##.#...##..\n...##...#..#..####.#........#..\n.........#...#......##..##..#..\n..#.....###.#..#..####.#.......\n.......#......#...#..##....#...\n.#.....#.#.#..#....#...#..##...\n..........#.#...#...#.#..#.....\n....#.....#........#.....##..#.\n..#.#.##.........#...##.....##.\n.........#.##....#............#\n............##.....#.......#.#.\n......#.....#...#..#..###......\n##.....#.......#...##.#....#...\n...........##......#..##...#.#.\n..#.#.#.#...#.......#....#...#.\n#.............#.....#.#...###..\n##....#.......#.....#..##.#....\n...#.......#....#.........##...\n......#.......#......##.##.....\n..#......#...#.#........#......\n....#.#....#.#.##......#.....#.\n#......#.........#..#....#.....\n........#..#....##.....#.......\n#......##....#.##....#......#..\n..#.......#............##.....#\n...........#...#...........#...\n#.......#...#....#....#...#.#.#\n..###..#........#........#.....\n..#..###...........#.#......#..\n.#...........#......#..........\n.#.......#.....#.......#.......\n..#......##.#............#....#\n#..........#.....#.#..#........\n.....#...##.##.......#......#..\n..........#.....#........#.#.#.\n....#......#.....#......#.#....\n.........#.#.#..#...##....#...#\n.........#.......#...##...#.#..\n.##........#...............#...\n.......#....#...........##.....\n.........###......#.........#.#\n......#.......#...#..........#.\n...#.#..........##......#...#..\n#.......#.#..........#.........\n................#..#......#..##\n.....#...#....#.#.....#........\n#.....#....#...........#....#..\n#....#.#..#...##....#...##.#...\n...#.....#......#.#....#..#..#.\n..#................#...#.#..##.\n..........#..............#..#.#\n.....##.....#..#.###...........\n....#.#......#.#...........#...\n.#....#.#.........##.#....#....\n.#.#........#........###....#..\n##.#................#...#..#...\n.......#......##..#.....#..#.#.\n...#............#......###...##\n#.#...........#.........#......\n.....#.#.#.................#...\n....#..............#...#.#.....\n...#.#.....##..#...............\n.#..##...#....##.....###..#....\n...............#...#...#.#.###.\n.###....#.....#...#.#......#...\n...#..#.....#.......#..##.#....\n...........#..#....##..#...#...\n...#...#..........#.......##.#.\n............#.#.......#........\n....#.........#.....#..........\n...#.###.##..#...##..####..#..#\n.#.#...#..#...................#\n.....#..#.....##..#............\n....#......#...##..#.##........\n...#...............#..#.....##.\n...#......#.........#.#..##....\n.#....#.##.......#......#......\n....#.......#....#..........#..\n#.#.#....###.#.#.............#.\n..##..###........#.#..#.#..#...\n......#.#............##.#...###\n.........#.#....#####.....##...\n............##......#.#..#.....\n...#.....#.....###....#........\n##..........####.##.#.#........\n....................##.....##.#\n#.#............#........#......\n....#...##.....#......#....#...\n...###.#..##..................#\n..###......#..............#.#.#\n.#...#...........#....#........\n....#............#..#..#.#.....\n...#.........#.#.........#.####\n..#...#...#...#...........#....\n...............#.#......##..#..\n#....#.#.......#.#..#......#..#\n........#.#....#..#...#..#...#.\n...#..#.......#...........#....\n...........#.......#...........\n.#......#................#.....\n....#.#.#...#......#..#...#....\n................#.#.#....#.....\n.........................##..#.\n.#...........#............##...\n#...............#.....##.#.#..#\n.........#.......###....#.....#\n....##...#...#.....#..#........\n........#.....#..#.#.#...#..#..\n......#.......#.#.........#.#..\n#......#............#...#....#.\n#..##...#..#................#..\n.##...#...#.....#.##.......#..#\n.......#.##........##..##......\n##.#..##...............#.....#.\n......#....#..##...#......###.#\n#........##..#....#.#......#...\n.#......##.#...#.#...#.........\n.#.#...#..#.............#......\n.##..........#..........#......\n.#.....#.....#..............#.#\n..#.........#..#.#.....#.#....#\n..#.##..............##...#..###\n....................#..........\n......###..#..#...........#....\n..#..........#.......#...#.....\n...#......#......#.............\n....##..............#.#.....#..\n........#.#......#..#........##\n.............#...#.#.........##\n...###...#..........##.......#.\n.#..........#...##..#.#.....#..\n##...#.........#...............\n......#....#....#.....#.....#..\n..........#....#...#...#..#...#\n...##....#.#.#..#...##.........\n#......#.#...##.###...#....#...\n##.......##.#......##..#...#...\n......#.............#.##.....##\n#.......###....####.#...##....#\n..#...#..#.......#..........#..\n#.....#..#..#..#.##...###...#..\n.....##.#..#..#..#.#....#...#..\n..#...#..................##....\n....#.#........##..............\n#...#.......##...#...#.#.......\n..#...#........##....#.#.......\n..........###...###...#......#.\n#.....#..###...##...##..#..#..#\n..#.....##.....#.......##..#.#.\n........#........#.........#...\n.................#....#.......#\n.......#...#.....#...#.#.......\n....##...............#...##...#\n.##...#................#...#...\n.............#.................\n.#..#....#....#.#....#.........\n.#.#..#..........#.......#.....\n.....##.....##...#..#..........\n#...#.#.........#......#..#....\n........#....#...#....#.#.##...\n....#..#........#...#...#......\n.#..#.....#.#...#.........#....\n.#..#..#...........#..#....#...\n....###.............#..#.......\n#......#..#..##..........#.#...\n#..#..#.##..#...#.#.#..........\n....###......#.##.....#....#...\n.##..#...#......##.#...........\n..#..#.......#.....#.##....#.#.\n.......#.#.#........#....##....\n..##...#....#...............###\n#..##..#...........#.#....##...\n...##..#.....................#.\n###......#....#....###..#...##.\n.........##............#..#...#\n..#..........#...#.#.#......#.#\n.......#.....##..##......#.##..\n#..........#.....##.#..........\n#.......#.#...#...#....#.......\n#...#.....##.......#.#..#.#.#..\n.........#.#.#..#..#...#.###...\n.................##...#....#...\n###.......#..........##...#....\n#.#..#.........#....##.#.......\n......#.#.....#........#.......\n.......#.#........#......#.#..#\n..............#..#...##....#..#\n#...........#...##.....#..#.#..\n..#....#..#.#.#...#..#....#.#..\n...##.#.....#..#...##..#.....#.\n..#.#................#........#\n......#...#.............#......\n.##............#....#...#..#...\n....#...#...........#.......#..\n.###..#.......#.............#.#\n.#.#....#.#...........#.#......\n...#.........#.........#..#....\n...#..........#.#.....#.#......\n.....#........#....##......#...\n..#.#.#......#..#.#......#....#\n.#.#..#................#.#.....\n.#.#.........##...#.......#.#.#\n#..#.....#...#..#...........#..\n..##......####......#..#....###\n#.....###....#.#........#..#..#\n..##.#...#.#..##..........#..#.\n#.........#.#.............#...#\n...#.#...#...#.#.#....##.......\n##.##...#.....#...#...........#\n....#........#.#.....#.........\n.................##..#..##...##\n.....##....#...#...#.....#..#..\n....#...#........#............#\n..#...........##....#...#...##.\n.....#......#.........#..##.#.."

  val splitData: Array[String] = data.split("\n")

 //println(splitData.mkString("Array(", ",", ")"))

  val slopeLength = splitData.length

  def route(data: Array[String], Right: Int, Down: Int): Int = {
    var trees = new ListBuffer[String]()
    for (z <- 0 until (slopeLength / Down)) {
      val xFactor = Right * (z + 1)
      val yFactor = Down * (z + 1)
      val x =
        if (xFactor >= 31) {
          if (xFactor == 31) 0
          else xFactor % 31
        }
        else xFactor
      val y =
        if (yFactor >= slopeLength) {
          if (yFactor == slopeLength) 0
          else yFactor % slopeLength
        }
        else yFactor
      val treeOrOpenSquare: Char = data(y).charAt(x)
      if (treeOrOpenSquare == '#') trees += "X"
    }
    trees.length
  }

  val oneByOne = BigInt(route(splitData, 1, 1))
  val threeByOne = BigInt(route(splitData, 3, 1))
  val fiveByOne = BigInt(route(splitData, 5, 1))
  val sevenByOne = BigInt(route(splitData, 7, 1))
  val oneByTwo = BigInt(route(splitData, 1, 2))

  println(oneByOne, threeByOne, fiveByOne, sevenByOne, oneByTwo)
  val answer: BigInt = oneByOne * threeByOne * fiveByOne * sevenByOne * oneByTwo
  println(answer)
}
