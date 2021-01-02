import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Using}

object Day4 extends App {

  val input = Using(Source.fromFile("src/main/scala/Input/Day4.txt")) {
    source => source.getLines().mkString(" ")
  }

  val passports: Array[Array[String]] = input match {
    case Success(data) =>
      val passportList = data.split("  ")
      val passportItems = for (passport <- passportList) yield {
        passport.split(" ")
      }
      passportItems
  }

  val passportsLength: Int = passports.length

  def containsCid(passport: Array[String]): Boolean = {
    if (passport.mkString("").contains("cid:")) true
    else false
  }

  def extract(passport: Array[String], itemName: String): String = {
    passport.filter(item => item.substring(0, 3) == itemName).mkString("").split(":")(1)
  }

  @tailrec
  def numberValid(passports: Array[Array[String]], indexNumber: Int = 0, total: Int = 0): Int = {
    if (indexNumber == passportsLength) total
    else {
      passports(indexNumber) match {
        case _ if passports(indexNumber).length < 7 => numberValid(passports, indexNumber + 1, total)
        case _ if passports(indexNumber).length == 7 && !containsCid(passports(indexNumber)) => numberValid(passports, indexNumber + 1, total + 1)
        case _ if passports(indexNumber).length == 7 && containsCid(passports(indexNumber)) => numberValid(passports, indexNumber + 1, total)
        case _ if passports(indexNumber).length == 8 => numberValid(passports, indexNumber + 1, total + 1)
      }
    }
  }

  def passportItemsValid(passport: Array[String]): Boolean = {
    val birthYear: Int = extract(passport, "byr").toInt
    val issueYear: Int = extract(passport, "iyr").toInt
    val expirationYear: Int = extract(passport, "eyr").toInt
    val height: String = extract(passport, "hgt")
    val hairColour: String = extract(passport, "hcl")
    val eyeColour: String = extract(passport, "ecl")
    val passportId: String = extract(passport, "pid")

    val birthYearValid =
      if (birthYear >= 1920 && birthYear <= 2002) true
      else false

    val issueYearValid =
      if (issueYear >= 2010 && issueYear <= 2020) true
      else false

    val expirationYearValid =
      if (expirationYear >= 2020 && expirationYear <= 2030) true
      else false

    val heightValid =
      if (height.contains("cm") && height.substring(0, height.indexOf('c')).toInt >= 150 && height.substring(0, height.indexOf('c')).toInt <= 193) true
      else if (height.contains("in") && height.substring(0, height.indexOf('i')).toInt >= 59 && height.substring(0, height.indexOf('i')).toInt <= 76) true
      else false

    val hairColourValid =
      if (hairColour.length == 7 && hairColour.charAt(0) == '#' && hairColour.substring(1, hairColour.length).matches("[a-f0-9]{6}")) true
      else false

    val eyeColourValid =
      if (eyeColour == "amb" || eyeColour == "blu" || eyeColour == "brn" || eyeColour == "gry" || eyeColour == "grn" || eyeColour == "hzl" || eyeColour == "oth") true
      else false

    val passportIdValid =
      if (passportId.length == 9 && passportId.matches("[0-9]{9}")) true
      else false

    if (birthYearValid && issueYearValid && expirationYearValid && heightValid && hairColourValid && eyeColourValid && passportIdValid) true
    else false

  }

  @tailrec
  def numberValidPartTwo(passports: Array[Array[String]], indexNumber: Int = 0, total: Int = 0): Int = {
    if (indexNumber == passportsLength) total
    else {
      passports(indexNumber) match {
        case _ if passports(indexNumber).length < 7 => numberValidPartTwo(passports, indexNumber + 1, total)
        case _ if passports(indexNumber).length == 7 && !containsCid(passports(indexNumber)) && passportItemsValid(passports(indexNumber)) => numberValidPartTwo(passports, indexNumber + 1, total + 1)
        case _ if passports(indexNumber).length == 7 && !containsCid(passports(indexNumber)) && !passportItemsValid(passports(indexNumber)) => numberValidPartTwo(passports, indexNumber + 1, total)
        case _ if passports(indexNumber).length == 7 && containsCid(passports(indexNumber)) => numberValidPartTwo(passports, indexNumber + 1, total)
        case _ if passports(indexNumber).length == 8 && passportItemsValid(passports(indexNumber)) => numberValidPartTwo(passports, indexNumber + 1, total + 1)
        case _ if passports(indexNumber).length == 8 && !passportItemsValid(passports(indexNumber)) => numberValidPartTwo(passports, indexNumber + 1, total)
      }
    }
  }

  println(numberValid(passports))
  println(numberValidPartTwo(passports))
}
