package advent
package y2022

import scala.io.Source

@main
def d20221202(): Unit = {
  val puzzleInput = "2022/20221202e.txt" // 20221202.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val score = Map("A X" -> 4, "A Y" -> 8, "A Z" -> 3,
    "B X" -> 1, "B Y" -> 5, "B Z" -> 9,
    "C X" -> 7, "C Y" -> 2, "C Z" -> 6)

  println("answer1: " + lines.map(score).sum)

  val shape = Map("A X" -> "A Z", "A Y" -> "A X", "A Z" -> "A Y",
    "B X" -> "B X", "B Y" -> "B Y", "B Z" -> "B Z",
    "C X" -> "C Y", "C Y" -> "C Z", "C Z" -> "C X")

  println("answer2: " + lines.map(shape).map(score).sum)
}
