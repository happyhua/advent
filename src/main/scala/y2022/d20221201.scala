package advent
package y2022

import scala.io.Source

@main
def d20221201(): Unit = {
  val puzzleInput = "2022/20221201e.txt" // 20221201.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val elfCalories = lines.foldLeft(Seq(0))(
    (calories, calorie) =>
      if (calorie.isEmpty)
        calories :+ 0
      else
        calories.init :+ calories.last + calorie.toInt
  )

  println("answer1: " + elfCalories.max)
  println("answer2: " + elfCalories.sorted.takeRight(3).sum)
}
