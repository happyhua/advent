package advent
package y2022

import scala.io.Source

@main
def d20221203(): Unit = {
  val puzzleInput = "2022/20221203e.txt" // 20221203.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val itemMap = (('a' to 'z').zip(1 to 26) ++ ('A' to 'Z').zip(27 to 52)).toMap

  val sum = lines.map(
    line =>
      assert(line.length % 2 == 0, line)

      val length = line.length >> 1

      val firstCompartment = line.substring(0, length)
      val secondCompartment = line.substring(length)

      val items = firstCompartment.intersect(secondCompartment)

      assert(items.distinct.length == 1, items)

      itemMap(items.head)
  ).sum

  println("answer1: " + sum)

  val groupedSum = lines.grouped(3).map(
    groupedLines =>
      val items = groupedLines.reduce((left, right) => left.intersect(right))

      assert(items.distinct.length == 1, items)

      itemMap(items.head)
  ).sum

  println("answer2: " + groupedSum)
}
