package advent
package y2022

import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221223(): Unit = {
  val puzzleInput = "2022/20221223e.txt" // 20221223.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  enum Direction {
    case N, S, W, E
  }

  val currentColumn = lines(0).size

  val extra = 500

  val emptyLine = ".".repeat(currentColumn)

  var input = (LazyList.fill(extra)(emptyLine) :++ lines :++ LazyList.fill(extra)(emptyLine)).zipWithIndex.flatMap(
    (line, row) =>
      (".".repeat(extra) + line + ".".repeat(extra)).zipWithIndex.map(
        (char, column) =>
          ((row + 1, column + 1), char == '#')
      )
  ).toMap

  val rows = input.keys.map(_._1)
  val columns = input.keys.map(_._2)

  val rowMin = rows.min
  val rowMax = rows.max
  val columnMin = columns.min
  val columnMax = columns.max

  println(rowMin + ", " + rowMax + ", " + columnMin + ", " + columnMax)

  var elves = input.filter(_._2).keys.toSet
  println("elves size: " + elves.size)

  var directions: Seq[Direction] = Direction.values.toSeq

  def isAllFree(elf: (Int, Int)): Boolean = {
    isFree(elf._1, elf._2 + 1) && isFree(elf._1, elf._2 - 1) && isFree(elf._1 + 1, elf._2 + 1) && isFree(elf._1 + 1, elf._2 - 1)
      && isFree(elf._1 - 1, elf._2 + 1) && isFree(elf._1 - 1, elf._2 - 1) && isFree(elf._1 + 1, elf._2) && isFree(elf._1 - 1, elf._2)
  }

  def isFree(elf: (Int, Int)): Boolean = {
    !inMap(elf) || !input(elf)
  }

  def inMap(point: (Int, Int)): Boolean = {
    point._1 >= rowMin && point._1 <= rowMax && point._2 >= columnMin && point._2 <= columnMax
  }

  def canMove(point: (Int, Int)): Boolean = {
    inMap(point) && !input(point)
  }

  def planMove(): Set[((Int, Int), (Int, Int))] = {
    //    val candidates = elves.filterNot(position => position._1 == rowMin || position._1 == rowMax
    //      || position._2 == columnMin || position._2 == columnMax)

    val candidates = elves.filterNot(isAllFree(_))

    candidates.flatMap(
      candidate =>
        var nextMove: Option[(Int, Int)] = None

        directions.takeWhile(
          direction =>
            direction match
              case Direction.N =>
                if (canMove(candidate._1 - 1, candidate._2) && canMove(candidate._1 - 1, candidate._2 - 1) &&
                  canMove(candidate._1 - 1, candidate._2 + 1))
                  nextMove = Some(candidate._1 - 1, candidate._2)
                else
                  nextMove = None
              case Direction.S =>
                if (canMove(candidate._1 + 1, candidate._2) && canMove(candidate._1 + 1, candidate._2 - 1) &&
                  canMove(candidate._1 + 1, candidate._2 + 1))
                  nextMove = Some(candidate._1 + 1, candidate._2)
                else
                  nextMove = None
              case Direction.W =>
                if (canMove(candidate._1, candidate._2 - 1) && canMove(candidate._1 - 1, candidate._2 - 1) &&
                  canMove(candidate._1 + 1, candidate._2 - 1))
                  nextMove = Some(candidate._1, candidate._2 - 1)
                else
                  nextMove = None
              case _ =>
                if (canMove(candidate._1, candidate._2 + 1) && canMove(candidate._1 - 1, candidate._2 + 1) &&
                  canMove(candidate._1 + 1, candidate._2 + 1))
                  nextMove = Some(candidate._1, candidate._2 + 1)
                else
                  nextMove = None

            nextMove.isEmpty
        )

        nextMove.map((candidate, _))
    )
  }

  def move(plan: Set[((Int, Int), (Int, Int))]): Unit = {
    // val expectedScenario: Set[(Int, Int)] = (elves.diff(plan.map(_._2)) :++ plan.map(_._2)).toSet

    //    plan.filter((_, move) => expectedScenario.contains(move)).foreach(println(_))

    plan.groupBy(_._2).filter(_._2.size == 1).values.foreach(
      _.foreach(
        (from, to) =>
          input = input.updated(from, false).updated(to, true)
          elves = elves - from + to
      )
    )
  }

  (1 to 10000).foreach(
    round =>
      // println("elves: " + elves)

      val plan = planMove()
      // println("plan move: " + plan)
      // println(plan.size)

      // elves.foreach(println(_))
      move(plan)
      // elves.foreach(println(_))

      directions = directions.tail :+ directions(0)

      if (plan.size == 0) {
        println("answer: " + round)
        return
      }
  )

  def printInput(): Unit = {
    val rows = input.groupBy(_._1._1).toSeq.sortBy(_._1)

    rows.foreach(
      row =>
        row._2.toSeq.sortBy(_._1._2).foreach(
          column => if (column._2) print('#') else print('.')
        )
        println()
    )
  }

  // printInput()

  val rowRange = (elves.map(_._1).min, elves.map(_._1).max)
  val columnRange = (elves.map(_._2).min, elves.map(_._2).max)

  var count = 0
  (rowRange._1 to rowRange._2).foreach(
    row =>
      (columnRange._1 to columnRange._2).foreach(
        column =>
          if (!elves.contains(row, column))
            count += 1
      )
  )

  println(rowRange)
  println(columnRange)
  println(count)
}
