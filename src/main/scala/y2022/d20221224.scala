package advent
package y2022

import util.printData

import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221224(): Unit = {
  val puzzleInput = "2022/20221224e.txt" // 20221224.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  def getCoordinateBlizzards(coordinate: (Int, Int), blizzards: Set[(Int, Int, Arrow)]): Seq[Arrow] = {
    Arrow.values.foldLeft(Seq[Arrow]())(
      (coordinateBlizzards, arrow) =>
        if (blizzards.contains(coordinate._1, coordinate._2, arrow))
          coordinateBlizzards :+ arrow
        else
          coordinateBlizzards
    )
  }

  def nextBlizzards(blizzards: Set[(Int, Int, Arrow)], rows: Int, columns: Int): Set[(Int, Int, Arrow)] = {
    blizzards.map(
      (row, column, arrow) =>
        arrow match
          case Arrow.Up =>
            var rowNext = row - 1
            if (rowNext == 1)
              rowNext = rows - 1

            (rowNext, column, arrow)
          case Arrow.Down =>
            var rowNext = row + 1
            if (rowNext == rows)
              rowNext = 2

            (rowNext, column, arrow)
          case Arrow.Left =>
            var columnNext = column - 1
            if (columnNext == 1)
              columnNext = columns - 1

            (row, columnNext, arrow)
          case Arrow.Right =>
            var columnNext = column + 1
            if (columnNext == columns)
              columnNext = 2

            (row, columnNext, arrow)
    )
  }

  def printValley(blizzards: Set[(Int, Int, Arrow)])(implicit path: Set[(Int, Int)]): Unit = {
    path.groupBy(_._1).toSeq.sortBy(_._1).foreach(
      (_, row) =>
        row.toSeq.sortBy(_._2).foreach(
          (rowIndex, columnIndex) =>
            val coordinateBlizzards = getCoordinateBlizzards((rowIndex, columnIndex), blizzards)

            if (coordinateBlizzards.size == 0)
              print(".")
            else if (coordinateBlizzards.size > 1)
              print(coordinateBlizzards.size)
            else
              print(coordinateBlizzards(0))
        )

        println()
    )
  }

  val rows = lines.size
  val columns = lines(0).size

  implicit var path: Set[(Int, Int)] = Set()
  var blizzards: Set[(Int, Int, Arrow)] = Set()
  lines.zipWithIndex.map(
    (row, rowIndex) =>
      row.zipWithIndex.filterNot(_._1 == '#').map(
        (char, columnIndex) =>
          path = path.+((rowIndex + 1, columnIndex + 1))

          char match
            case '^' =>
              blizzards = blizzards + ((rowIndex + 1, columnIndex + 1, Arrow.Up))
            case 'v' =>
              blizzards = blizzards + ((rowIndex + 1, columnIndex + 1, Arrow.Down))
            case '<' =>
              blizzards = blizzards + ((rowIndex + 1, columnIndex + 1, Arrow.Left))
            case '>' =>
              blizzards = blizzards + ((rowIndex + 1, columnIndex + 1, Arrow.Right))
            case _ =>
      )
  )

  println(rows + ", " + columns)
  // printValley(blizzards)
  printData(path,
    (rowIndex, columnIndex, present) =>
      if (present)
        val coordinateBlizzards = getCoordinateBlizzards((rowIndex, columnIndex), blizzards)
        if (coordinateBlizzards.isEmpty)
          print('.')
        else if (coordinateBlizzards.size > 1)
          print(coordinateBlizzards.size)
        else
          print(coordinateBlizzards(0))
      else
        print('#')
    , columnRangeOption = Some(1, columns)
  )

  val start = path.find(_._1 == 1).get
  val end = path.find(_._1 == rows).get
  println("start: " + start)
  println("end: " + end)

  val startNext = (start._1 + 1, start._2)
  val endNext = (end._1 - 1, end._2)
  println("startNext: " + startNext)
  println("endNext: " + endNext)

  var blizzardsSeq = (1 to 1000).foldLeft(Seq(blizzards))(
    (seq, _) =>
      seq :+ nextBlizzards(seq.last, rows, columns)
  )

  def next(coordinate: (Int, Int)): Seq[(Int, Int)] = {
    var next = Seq(coordinate)

    if (coordinate == start) {
      next = next :+ startNext
    } else if (coordinate == end) {
      next = next :+ endNext
    }
    else {
      if (coordinate._1 > 2)
        next = next :+ (coordinate._1 - 1, coordinate._2)

      if (coordinate._1 < rows - 1)
        next = next :+ (coordinate._1 + 1, coordinate._2)

      if (coordinate._2 > 2)
        next = next :+ (coordinate._1, coordinate._2 - 1)

      if (coordinate._2 < columns - 1)
        next = next :+ (coordinate._1, coordinate._2 + 1)
    }

    next
  }

  def search(coordinate: (Int, Int), index: Int, blizzardsSeq: Seq[Set[(Int, Int, Arrow)]]): Int = {
    if (coordinate == endNext)
      index
    else if (index == blizzardsSeq.size || !getCoordinateBlizzards(coordinate, blizzardsSeq(index)).isEmpty)
      0
    else {
      next(coordinate).map(
        nextCoordinate => search(nextCoordinate, index + 1, blizzardsSeq)
      ).max
    }
  }

  def canContinue(coordinate: (Int, Int), index: Int, blizzardsSeq: Seq[Set[(Int, Int, Arrow)]]): Boolean = {
    index < blizzardsSeq.size && getCoordinateBlizzards(coordinate, blizzardsSeq(index)).isEmpty
  }

  def run(from: (Int, Int), to: (Int, Int)): Option[Int] = {
    var candidates = Set(from)
    (1 to blizzardsSeq.size).map(
      minute =>
        candidates = candidates.flatMap(next(_)).filter(coordinate => canContinue(coordinate, minute, blizzardsSeq))

        // println(minute + ", " + candidates.size)

        if (candidates.contains(to))
          minute + 1
        else
          0
    ).find(_ > 0)
  }

  val trip1 = run(start, endNext).get
  println("answer1: " + trip1)

  blizzardsSeq = blizzardsSeq.drop(trip1)
  // printValley(blizzardsSeq(0))
  val trip2 = run(end, startNext).get
  println(trip2)

  blizzardsSeq = blizzardsSeq.drop(trip2)
  val trip3 = run(start, endNext).get
  println(trip3)

  println(trip1 + trip2 + trip3)

  // println(search(start, 0, blizzardsSeq) + 1)
  // blizzardsSeq.foreach(printValley(_))
}

enum Arrow(char: Char) {
  case Up extends Arrow('^')
  case Down extends Arrow('v')
  case Left extends Arrow('<')
  case Right extends Arrow('>')

  override def toString: String = char.toString
}
