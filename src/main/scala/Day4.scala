import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Cell(val value: Int, var isMarked: Boolean = false) {
  override def toString: String = s"$value - $isMarked"
}

class Board() {
  var cells: Array[Array[Cell]] = _
  var isWin: Boolean = false

  def this(lines: List[String]) = {
    this()
    cells = initialize(lines)
  }

  def mark(number: Int): Option[(Int, Int)] = {
    var i = 0
    while i < cells.length
    do
      val row = cells(i)
      var j = 0
      while j < row.length && row(j).value != number do j += 1
      if j < row.length then {
        row(j).isMarked = true
        return Some(i, j)
      }
      i += 1
    None
  }

  def initialize(lines: List[String]): Array[Array[Cell]] = {
    val cells = new Array[Array[Cell]](5)
    for (i <- 0 until 5) {
      val line = lines(i)
      val numbers = line.trim().split("\\s+")
      cells(i) = new Array[Cell](5)
      for (j <- 0 until 5) {
        cells(i)(j) = new Cell(numbers(j).toInt)
      }
    }
    cells
  }

  def checkWinHorizontalLine(y: Int): Boolean = {
    val row = cells(y)
    if row.forall(_.isMarked) then
      isWin = true;
      true
    else
      false
  }

  def checkWinVerticalLine(x: Int): Boolean = {
    val column = cells(0)(x) :: cells(1)(x) :: cells(2)(x) :: cells(3)(x) :: cells(4)(x) :: Nil
    if column.forall(_.isMarked) then
      isWin = true
      true
    else
      false
  }

  def calculateWin(number: Int): Int = {
    var i = 0
    var sum = 0
    while i < cells.length
    do
      val row = cells(i)
      var j = 0
      while j < row.length
      do
        sum += (if row(j).isMarked then 0 else row(j).value)
        j += 1
      i += 1

    sum * number
  }
}

def parseNumberDraw(line: String): Array[Int] = {
  val numbers = line.split(",")
  numbers.map(_.toInt)
}

def playBingoPart1(numbers: Array[Int], boards: List[Board]): Option[Int] = {
  var wins = 0
  for (number <- numbers) {
    boards.foreach(board => {
      val coordinates = board.mark(number)
      if coordinates.isDefined &&
        (board.checkWinHorizontalLine(coordinates.get._1) ||
          board.checkWinVerticalLine(coordinates.get._2))
      then
        return Some(board.calculateWin(number))
    })
  }
  None
}

def playBingoPart2(numbers: Array[Int], boards: List[Board]): Option[Int] = {
  var wins = 0
  for (number <- numbers) {
    boards.foreach(board => {
      if !board.isWin then
        val coordinates = board.mark(number)
        if coordinates.isDefined &&
          (board.checkWinHorizontalLine(coordinates.get._1) ||
            board.checkWinVerticalLine(coordinates.get._2))
        then
          if wins + 1 == boards.length then
            return Some(board.calculateWin(number))
          else wins += 1
    })
  }
  None
}

@main def day4(): Unit = {
  val file = Source.fromResource("day4/example.txt")
  val lines = file.getLines().toList
  val numbers = parseNumberDraw(lines.head)

  val boardLines = lines.tail.filter(_.nonEmpty).map(_.trim)
  val boards = mutable.ArrayBuffer[Board]()
  for (i <- 0 until boardLines.length / 5) {
    val list = boardLines.slice(i * 5, i * 5 + 5)
    boards.addOne(Board(list))
  }

  val result = playBingoPart1(numbers, boards.toList)
  println(result.get)
}
