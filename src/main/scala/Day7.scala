import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.{Success, Using}

object Day7 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day7.txt")) {
    source => source.getLines().mkString("").split("\\.")
  }

  val rules = input match {
    case Success(data) => data
  }

  @tailrec
  def numGoldBags(rules: Array[String], indexNum: Int = 0, answer: Int = 0, colour: String = "wavy yellow bags", colourList: String = ""): Array[String] = {
    if (indexNum == rules.length - 1) colourList.split(", ")
    else if (rules(indexNum).contains(colour)) numGoldBags(rules, indexNum + 1, answer + 1, colour, colourList + ", " + getColourName(rules(indexNum)))
    else numGoldBags(rules = rules, indexNum = indexNum + 1, answer = answer, colour = colour, colourList = colourList)
  }

  @tailrec
  def numContainGold(rules: Array[String], rulesIndexNum: Int, colourIndex: Int, colourList: List[String] = List("shiny gold bag")): Int = {
    if (colourIndex == colourList.length) colourList.length - 1
    else if (rulesIndexNum == rules.length) {
      numContainGold(rules, rulesIndexNum = 0, colourIndex + 1, colourList = colourList)
    }
    else if (rules(rulesIndexNum).contains(colourList(colourIndex)) && !colourList.contains(getColourName(rules(rulesIndexNum)))) {
      numContainGold(rules, rulesIndexNum + 1, colourIndex = colourIndex, colourList = colourList ::: List(getColourName(rules(rulesIndexNum))))
    }
    else if (rules(rulesIndexNum).contains(colourList(colourIndex)) && colourList.contains(getColourName(rules(rulesIndexNum)))) {
      numContainGold(rules, rulesIndexNum + 1, colourIndex = colourIndex, colourList = colourList)
    }
    else numContainGold(rules, rulesIndexNum = rulesIndexNum + 1, colourIndex = colourIndex, colourList = colourList)
  }

  @tailrec
  def partTwo(rules: Array[String], firstListIndex: Int, secondListIndex: Int, colourList: List[List[String]] = List(List("x shiny gold bags"))): Any = {
    println(s" firstIndex: $firstListIndex, secondIndex: $secondListIndex, $colourList")
    if (firstListIndex == colourList.length) colourList
    else if (secondListIndex == colourList(firstListIndex).length) partTwo(rules, firstListIndex + 1, secondListIndex = 0, colourList = colourList)
    else if (findColourBagRule(colourList(firstListIndex)(secondListIndex)).contains("no other bags")) {
      partTwo(rules, firstListIndex = firstListIndex, secondListIndex + 1, colourList = colourList)
    }
    else {
      val rule = findColourBagRule(colourList(firstListIndex)(secondListIndex))
      partTwo(rules = rules, firstListIndex = firstListIndex, secondListIndex = secondListIndex + 1, colourList = colourList ::: List(getBagContent(rule)))
    }
  }

  def part2(rules: Array[String], firstListIndex: Int, secondListIndex: Int, colourList: List[List[String]] = List(List("x shiny gold bags")), colourMap: mutable.Map[String, String] = collection.mutable.Map[String, String]()): mutable.Map[String, String] = {
    if (firstListIndex == colourList.length) colourMap
    else if (secondListIndex == colourList(firstListIndex).length) part2(rules, firstListIndex + 1, secondListIndex = 0, colourList = colourList, colourMap = colourMap)
    else if (findColourBagRule(colourList(firstListIndex)(secondListIndex)).contains("no other bags")) {
      val rule = findColourBagRule(colourList(firstListIndex)(secondListIndex))
      part2(rules, firstListIndex = firstListIndex, secondListIndex + 1, colourList = colourList, colourMap += (getColourName(rule) -> getBagContentString(rule)))
    }
    else {
      val rule = findColourBagRule(colourList(firstListIndex)(secondListIndex))
      part2(rules = rules, firstListIndex = firstListIndex, secondListIndex = secondListIndex + 1, colourList = colourList ::: List(getBagContent(rule)), colourMap += (getColourName(rule) -> getBagContentString(rule)))
    }
  }

  def getColourName(rule: String): String = {
    val colour = rule.split(" contain")(0)
    val singleColour = colour.replace("bags", "bag")
    singleColour
  }

  def getBagContent(rule: String): List[String] = {
    val colour = rule.split(" contain")(1)
    val format = colour.split(",")
    val answer = for (bags <- format) yield {
      bags.replaceFirst("\\s", "")
    }
    answer.toList
  }

  def getBagContentString(rule: String): String = {
    rule.split("contain ")(1)
  }

  def findColourBagRule(colour: String): String = {
    val formatColour = colour.substring(2)
    if (formatColour.charAt(formatColour.length - 1) != 's') {
      rules.filter(_.contains(formatColour + "s" + " contain")).mkString("")
    }
    else rules.filter(_.contains(formatColour + " contain")).mkString("")
  }

  def calculateBagNum(colourMap: mutable.Map[String, String], currentColour: String): Int = {
    val bagList = colourMap(currentColour).split(", ")
    val totalArray = for (bag <- bagList if !bag.contains("no other bags")) yield {
      val colourName = bag.substring(2)
      println(colourName)
      val formatColourName =
        if (colourName.charAt(colourName.length - 1) == 's') bag.substring(2, colourName.length + 1)
        else colourName
      val total: Int = {
        println(bag.charAt(0).toString.toInt)
        bag.charAt(0).toString.toInt + (bag.charAt(0).toString.toInt * calculateBagNum(colourMap, currentColour = formatColourName))
      }
      total
    }
    totalArray.sum
  }

  val answer = part2(rules, 0, 0)

  val testMap = mutable.Map(
    "shiny gold bag" -> "2 dark red bags",
    "dark red bag" -> "2 dark orange bags",
    "dark orange bag" -> "2 dark yellow bags",
    "dark yellow bag" -> "2 dark green bags",
    "dark green bag" -> "2 dark blue bags",
    "dark blue bag" -> "2 dark violet bags",
    "dark violet bag" -> "no other bags"
  )

  println(calculateBagNum(testMap, currentColour = "shiny gold bag"))
  println(calculateBagNum(answer, currentColour = "shiny gold bag"))
}
