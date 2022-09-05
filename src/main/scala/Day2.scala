import scala.io.Source

def calculatePosition(movements: List[(String, Int)]): Int = {
  var depth = 0
  var position = 0

  movements.foreach(movement => {
    movement._1 match
      case "up" => depth -= movement._2
      case "down" => depth += movement._2
      case _ => position += movement._2
  })

  depth * position
}

def calculatePosition2(movements: List[(String, Int)]): Int = {
  var depth = 0
  var position = 0
  var aim = 0

  movements.foreach(movement => {
    movement._1 match
      case "up" => aim -= movement._2
      case "down" => aim += movement._2;
      case _ => position += movement._2; depth += (aim * movement._2);
  })

  depth * position
}


@main def day2(): Unit = {
  val file = Source.fromResource("day2/input.txt")
  val commands = file.getLines().toList
  val movements = commands.map(command => {
    val values = command.split(" ")
    (values(0), values(1).toInt)
  })
  val position = calculatePosition2(movements)
  println(position)
}