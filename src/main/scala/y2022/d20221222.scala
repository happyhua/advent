package advent
package y2022

import util.printData

import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221222(): Unit = {
  val puzzleInput = "2022/20221222e.txt" // 20221222.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val inputMap = lines.dropRight(2)
  val instructionsText = lines.last

  val xMax = inputMap.map(_.size).max
  val yMax = inputMap.size

  println(xMax + ", " + yMax)

  val inputSeq = inputMap.zipWithIndex.flatMap(
    (line, row) =>
      line.zipWithIndex.filterNot(_._1 == ' ').map(
        (char, column) =>
          if (char == '.')
            (column + 1, row + 1) -> false
          else {
            assert(char == '#')
            (column + 1, row + 1) -> true
          }
      )
  )

  val input = inputSeq.toMap

  val monkeyMap = MonkeyMap(input, xMax, yMax)

  // input.foreach(println(_))
  val regex = "([0-9]+)|([RL])|([0-9]+)$".r
  val instructions = regex.findAllIn(instructionsText).map(
    movement =>
      val intOption = movement.toIntOption
      if (intOption.isDefined)
        intOption.get
      else
        movement == "R"
  ).toSeq

  var current = (1, 1, 0)
  var directions = 4
  instructions.foreach(
    instruction =>
      instruction match
        case right: Boolean =>
          var facingNew = (current._3 + (if (right) 1 else -1)) % directions
          if (facingNew < 0)
            facingNew += directions
          current = (current._1, current._2, facingNew)
        case movement: Int =>
          println("current: " + current + ", move " + movement)

          var step = 0
          var next = monkeyMap.next(current)
          while (next.isDefined && step < movement) {
            current = next.get

            next = monkeyMap.next(current)
            step += 1
          }
  )

  println(current)
  println(current._2 * 1000 + current._1 * 4 + current._3)

  //  input.foreach(println(_))
}

case class MonkeyMap(input: Map[(Int, Int), Boolean], xMax: Int, yMax: Int) {
  private var directionMap: Map[(Int, Int, Int), (Int, Int, Int)] = Map()

  // TODO, this is a hardcoded, not a generic solution.
  // Switch between directionMapE and directionMapR, E for example data, R for your real data
  directionMap = directionMapE()

  println(directionMap.keys.map(data => (data._1, data._2)).toSeq.distinct.diff(directionMap.values.map(data =>
    (data._1, data._2)).toSeq.distinct).sortBy(_._2))
  println(directionMap.values.map(data => (data._1, data._2)).toSeq.distinct.size)
  println(directionMap.keys.map(data => (data._1, data._2)).toSeq.distinct.size)

  def next(from: (Int, Int, Int)): Option[(Int, Int, Int)] = {
    if (directionMap.contains(from)) {
      val check = directionMap(from)

      if (input((check._1, check._2)))
        None
      else
        Some(check)
    } else {
      var nextX = from._1
      var nextY = from._2

      from._3 match
        case 0 =>
          nextX += 1
          if (nextX == xMax + 1)
            nextX = 0
        case 1 =>
          nextY += 1
          if (nextY == yMax + 1)
            nextY = 0
        case 2 =>
          nextX -= 1
          if (nextX == 0)
            nextX = xMax
        case _ =>
          nextY -= 1
          if (nextY == 0)
            nextY = yMax

      val check = (nextX, nextY)

      if (!input.contains(check))
        next((nextX, nextY, from._3))
      else if (input(check))
        None
      else
        Some((nextX, nextY, from._3))
    }
  }

  private def directionMapR(): Map[(Int, Int, Int), (Int, Int, Int)] = {
    (51 to 100).map(
      index => (index, 1, 3) -> (1, 100 + index, 0)
    ).toMap ++
      (101 to 150).map(
        index => (index, 1, 3) -> (-100 + index, 200, 3)
      ).toMap ++
      (101 to 150).map(
        index => (index, 50, 1) -> (100, -50 + index, 2)
      ).toMap ++
      (1 to 50).map(
        index => (index, 101, 3) -> (51, 50 + index, 0)
      ).toMap ++
      (51 to 100).map(
        index => (index, 150, 1) -> (50, 100 + index, 2)
      ).toMap ++
      (1 to 50).map(
        index => (index, 200, 1) -> (100 + index, 1, 1)
      ).toMap ++
      (101 to 150).map(
        index => (1, index, 2) -> (51, 151 - index, 0)
      ).toMap ++
      (151 to 200).map(
        index => (1, index, 2) -> (-100 + index, 1, 1)
      ).toMap ++
      (1 to 50).map(
        index => (51, index, 2) -> (1, 151 - index, 0)
      ).toMap ++
      (51 to 100).map(
        index => (51, index, 2) -> (-50 + index, 101, 1)
      ).toMap ++
      (151 to 200).map(
        index => (50, index, 0) -> (-100 + index, 150, 3)
      ).toMap ++
      (51 to 100).map(
        index => (100, index, 0) -> (50 + index, 50, 3)
      ).toMap ++
      (101 to 150).map(
        index => (100, index, 0) -> (150, 151 - index, 2)
      ).toMap ++
      (1 to 50).map(
        index => (150, index, 0) -> (100, 151 - index, 2)
      ).toMap
  }

  private def directionMapE(): Map[(Int, Int, Int), (Int, Int, Int)] = {
    (9 to 12).map(
      index => (index, 1, 3) -> (13 - index, 5, 1)
    ).toMap ++
      (1 to 4).map(
        index => (index, 5, 3) -> (13 - index, 5, 1)
      ).toMap ++
      (5 to 8).map(
        index => (index, 5, 3) -> (9, -4 + index, 0)
      ).toMap ++
      (1 to 4).map(
        index => (index, 8, 1) -> (12, 13 - index, 3)
      ).toMap ++
      (5 to 8).map(
        index => (index, 8, 1) -> (9, 17 - index, 0)
      ).toMap ++
      (13 to 16).map(
        index => (index, 9, 3) -> (12, 21 - index, 2)
      ).toMap ++
      (9 to 12).map(
        index => (index, 12, 1) -> (13 - index, 8, 3)
      ).toMap ++
      (13 to 16).map(
        index => (index, 12, 1) -> (1, 21 - index, 0)
      ).toMap ++
      (5 to 8).map(
        index => (1, index, 2) -> (21 - index, 12, 3)
      ).toMap ++
      (1 to 4).map(
        index => (9, index, 2) -> (4 + index, 5, 1)
      ).toMap ++
      (9 to 12).map(
        index => (9, index, 2) -> (17 - index, 8, 3)
      ).toMap ++
      (1 to 4).map(
        index => (12, index, 0) -> (16, 13 - index, 2)
      ).toMap ++
      (5 to 8).map(
        index => (12, index, 0) -> (21 - index, 9, 1)
      ).toMap ++
      (9 to 12).map(
        index => (16, index, 0) -> (12, 13 - index, 2)
      ).toMap
  }
}

def testNewSolution(): Unit = {
  val lines = Source.fromResource("2022/20221222e.txt").getLines().toSeq

  val inputDataLines = lines.dropRight(2)
  val inputInstructionsLine = lines.last

  val rowMax = inputDataLines.size
  val columnMax = inputDataLines.map(_.size).max

  val dataSet = inputDataLines.zipWithIndex.flatMap(
    (line, rowIndex) =>
      line.zipWithIndex.filterNot(_._1 == ' ').map(
        (char, columnIndex) =>
          if (char == '.')
            (rowIndex + 1, columnIndex + 1) -> false
          else {
            assert(char == '#')
            (rowIndex + 1, columnIndex + 1) -> true
          }
      )
  ).toMap

  printData(dataSet.keys,
    (row, column, present) =>
      if (present)
        if (dataSet(row, column))
          print('#')
        else
          print('.')
      else
        print(' ')
  )

  // TODO, how to determine the rotation of a cube?
}
