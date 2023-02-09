package advent
package y2022

import scala.io.Source

@main
def d20221205(): Unit = {
  val puzzleInput = "2022/20221205e.txt" // 20221205.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val stackIndex = lines.zipWithIndex.find(
    (line, _) => line.isEmpty
  ).get._2

  val positions = lines(stackIndex - 1).zipWithIndex.flatMap(
    (char, index) =>
      if (char.isDigit)
        Some(index)
      else
        None
  )

  def moveStacks(reverse: Boolean): String = {
    var stacks = positions.map(
      position =>
        lines.take(stackIndex - 1).reverse.flatMap(
          line =>
            if (line(position) == ' ') None
            else Some(line(position))
        )
    )

    lines.slice(stackIndex + 1, lines.size).foreach(
      line =>
        val split = line.split(' ')

        val size = split(1).toInt
        val from = split(3).toInt - 1
        val to = split(5).toInt - 1

        var move = stacks(from).takeRight(size)
        if (reverse)
          move = move.reverse

        stacks = stacks.updated(from, stacks(from).dropRight(size))
        stacks = stacks.updated(to, stacks(to).appendedAll(move))
    )

    stacks.map(_.last).mkString
  }

  println("answer1: " + moveStacks(true))
  println("answer2: " + moveStacks(false))
}
