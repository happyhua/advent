package advent
package y2022

import scala.io.Source

@main
def d20221210(): Unit = {
  val puzzleInput = "2022/20221210e.txt" // 20221210.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val signals = lines.flatMap(
    line =>
      val split = line.split(" ")

      if (split(0) == "addx")
        Seq(0, split(1).toInt)
      else
        Seq(0)
  ).scanLeft(1)(
    (sum, line) =>
      sum + line
  ).zipWithIndex

  // signals.foreach(println)

  print("answer1: ")
  println(signals.filter(signal => (signal._2 + 1) % 40 == 20).map(signal => signal._1 * (signal._2 + 1)).sum)

  println("answer2: ")
  signals.init.foreach(
    signal =>
      val cycle = signal._2 % 40

      if (cycle >= signal._1 - 1 && cycle <= signal._1 + 1)
        print('#')
      else
        print('.')

      if ((signal._2 + 1) % 40 == 0)
        println()
  )
}
