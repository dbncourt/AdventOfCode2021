import scala.collection.mutable
import scala.io.Source

def calculatePowerConsumption(entries: Array[Array[Char]]): Int = {
  val average = new mutable.StringBuilder()
  var i = 0
  val wordLength = entries(0).length
  while i < wordLength
  do
    var ones = 0
    for (y <- entries.indices) {
      if entries(y)(i) == '1' then ones += 1
    }
    val common = if ones >= entries.length / 2 then '1' else '0'
    average.append(common)
    i += 1

  val gammaRate = parseInt(average.toString())
  val reversedAverage = average.mapInPlace(char => if char == '1' then '0' else '1').toString()
  val epsilonRate = parseInt(reversedAverage)
  gammaRate * epsilonRate
}

def calculateLifeSupportRating(entries: Array[Array[Char]]): Int = {
  val oxygen = (ones: Int, length: Int, char: Char) => if ones >= (length - ones) then char == '1' else char == '0'
  val oxygenRating = calculateRating(entries, oxygen)
  val co2 = (ones: Int, length: Int, char: Char) => if ones >= (length - ones) then char == '0' else char == '1'
  val co2Rating = calculateRating(entries, co2)
  oxygenRating * co2Rating
}

def calculateRating(entries: Array[Array[Char]], filer: (Int, Int, Char) => Boolean): Int = {
  var i = 0
  var common = entries.clone()
  val wordLength = entries(0).length
  while i < wordLength && common.length != 1
  do
    val length = common.length
    val ones = common.count(word => word(i) == '1')
    common = common.filter(word => filer(ones, length, word(i)))
    i += 1
  parseInt(common(0).mkString("", "", ""))
}

def parseInt(value: String): Int = {
  val trimmed = value.dropWhile(char => char == '0')
  Integer.parseInt(trimmed, 2)
}

@main def day3(): Unit = {
  val file = Source.fromResource("day3/example.txt")
  val entries = file.getLines().map(_.toCharArray).toArray

  println(calculatePowerConsumption(entries))
  println(calculateLifeSupportRating(entries))
}
