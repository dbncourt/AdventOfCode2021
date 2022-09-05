import scala.io.Source

class Day1 {

  def countIncreases(numbers: Array[Int]): Int = {
    var change = 0
    var previous = Int.MinValue
    for (number <- numbers) {
      if number > previous then
        change += 1
      previous = number
    }
    change - 1
  }

   def countGroupedIncreases(numbers: Array[Int]): Int = {
    var change = 0
    var next = numbers(1)
    var current = numbers(0) + next
    var previous = Int.MaxValue

    for (i <- 2 until numbers.length) {
      current = current + numbers(i)
      if (current > previous)
        change += 1

      previous = current
      current = next + numbers(i)
      next = numbers(i)
    }
    change
  }
}


@main def main(): Unit = {
  val day1 = Day1()
  val file = Source.fromResource("day1/input.txt")
  val numbers = file.getLines().map(line => line.toInt).toArray
  val part1 = day1.countIncreases(numbers)
  println(s"Part 1 increases: $part1")
  val part2 = day1.countGroupedIncreases(numbers)
  println(s"Part 1 increases: $part2")
}