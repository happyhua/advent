package advent
package y2022

import scala.io.Source

@main
def d20221209(): Unit = {
  val puzzleInput = "2022/20221209e.txt" // 20221209.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  enum Direction {
    case U, D, L, R
  }

  case class Coordinate(row: Int, column: Int) {
    def move(direction: Direction): Coordinate = {
      direction match
        case Direction.U => Coordinate(row - 1, column)
        case Direction.D => Coordinate(row + 1, column)
        case Direction.L => Coordinate(row, column - 1)
        case Direction.R => Coordinate(row, column + 1)
    }

    def move(direction: Direction, step: Int): Seq[Coordinate] = {
      (1 to step).scanLeft(this)(
        (head, _) => head.move(direction)
      ).tail
    }

    def pull(tail: Coordinate): Coordinate = {
      val newRow =
        if ((row - tail.row).abs > 1)
          tail.row + (if (row > tail.row) 1 else -1)
        else
          row
      val newColumn =
        if ((column - tail.column).abs > 1)
          tail.column + (if (column > tail.column) 1 else -1)
        else
          column

      Coordinate(newRow, newColumn)
    }

    def adjacent(other: Coordinate): Boolean = (row - other.row).abs < 2 && (column - other.column).abs < 2
  }

  val start = Coordinate(0, 0)

  val heads = lines.foldLeft(Seq(start))(
    (coordinates, line) =>
      val split = line.split(" ")

      val direction = Direction.valueOf(split(0))
      val step = split(1).toInt

      coordinates :++ coordinates.last.move(direction, step)
  )

  def pullTails(size: Int): Seq[Coordinate] = {
    (1 to size).foldLeft(heads)(
      (_heads, _) =>
        _heads.scanLeft(start)(
          (tail, head) =>
            if (head.adjacent(tail))
              tail
            else
              head.pull(tail)
        )
    )
  }

  println("answer1: " + pullTails(1).distinct.size)
  println("answer2: " + pullTails(9).distinct.size)
}
