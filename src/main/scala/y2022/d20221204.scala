package advent
package y2022

import scala.io.Source

@main
def d20221204(): Unit = {
  val puzzleInput = "2022/20221204e.txt" // 20221204.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val input = lines.map(
    line =>
      val split = line.split(',')

      val left = split(0).split('-')
      val right = split(1).split('-')

      ((left(0).toInt, left(1).toInt), (right(0).toInt, right(1).toInt))
  )

  val answer1Count = input.count(
    (left, right) =>
      left(0) >= right(0) && left(1) <= right(1) || right(0) >= left(0) && right(1) <= left(1)
  )
  println("answer1: " + answer1Count)

  println("answer2: " + input.count((left, right) => left(0) <= right(1) && left(1) >= right(0)))
}
