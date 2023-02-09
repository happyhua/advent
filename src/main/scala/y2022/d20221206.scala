package advent
package y2022

import scala.collection.mutable
import scala.io.Source

@main
def d20221206(): Unit = {
  val puzzleInput = "2022/20221206e.txt" // 20221206.txt is my real puzzle input data

  val input = Source.fromResource(puzzleInput).mkString

  def distinct(size: Int): Seq[Char] = {
    val result = mutable.Buffer[Char]()

    input.takeWhile(
      char =>
        result += char
        result.takeRight(size).distinct.size < size
    )

    result.toSeq
  }

  println("answer1: " + distinct(4).size)
  println("answer2: " + distinct(14).size)
}
