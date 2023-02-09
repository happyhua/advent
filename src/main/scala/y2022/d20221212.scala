package advent
package y2022

import scala.io.Source

@main
def d20221212(): Unit = {
  val puzzleInput = "2022/20221212e.txt" // 20221212.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  case class Coordinate(row: Int, column: Int, height: Int) {
    def canClimb(other: Coordinate): Boolean = height + 1 >= other.height

    def neighbours(coordinates: Seq[Coordinate]): Seq[Coordinate] = {
      coordinates.filter(coordinate => (row - coordinate.row).abs == 1 && column == coordinate.column
        || (column - coordinate.column).abs == 1 && row == coordinate.row)
    }
  }

  var start = Coordinate(0, 0, 0)
  var end = start
  val coordinates = lines.zipWithIndex.flatMap(
    (line, row) =>
      line.zipWithIndex.map(
        (char, column) =>
          val coordinate = Coordinate(row, column
            , (char match
              case 'S' => 'a'
              case 'E' => 'z'
              case _ => char).asDigit)

          char match
            case 'S' => start = coordinate
            case 'E' => end = coordinate
            case _ =>

          coordinate
      ))

  def getPath(start: Coordinate, climb: (Coordinate, Coordinate) => Boolean, found: Seq[Coordinate] => Boolean): Int = {
    var paths = Seq(Seq(start))
    var visited = Set(start)
    var path = paths.find(found(_))

    while (path.isEmpty) {
      paths = paths.flatMap(
        path =>
          val neighbours = path.last.neighbours(coordinates).filter(
            neighbour => climb(path.last, neighbour) && !visited.contains(neighbour))

          visited = visited ++ neighbours

          neighbours.map(path :+ _)
      )

      path = paths.find(found(_))
    }

    path.get.size - 1
  }

  println("answer1: " + getPath(start, (from, to) => from.canClimb(to), _.contains(end)))

  val starts = coordinates.filter(_.height == 'a'.asDigit)
  println("answer2: " + getPath(end, (from, to) => to.canClimb(from), path => starts.exists(path.contains(_))))
}
