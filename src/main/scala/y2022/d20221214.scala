package advent
package y2022

import scala.io.Source

@main
def d20221214(): Unit = {
  val puzzleInput = "2022/20221214e.txt" // 20221214.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val input = lines.flatMap(
    line =>
      val paths = line.split(" -> ")
        .map(_.split(","))
        .map(pair => (pair(0).toInt, pair(1).toInt))

      paths.tail.foldLeft(Seq(paths.head))(
        (path, pair) =>
          val (lastX, lastY) = path.last
          val (pairX, pairY) = pair

          assert(lastX == pairX || lastY == pairY, (lastX, lastY).toString() + ", " + pair.toString() + ", " + path)

          val pairs = if (lastX == pairX) {
            val x = lastX

            val step = if (lastY < pairY) 1 else -1
            (lastY to pairY by step).map((x, _)).tail
          } else {
            val y = lastY

            val step = if (lastX < pairX) 1 else -1
            (lastX to pairX by step).map((_, y)).tail
          }

          path :++ pairs
      )
  ).distinct

  val yMax = input.map(_._2).max
  val start = (500, 0)

  def run(break: Int => Boolean, continue: ((Int, Int), Boolean) => Boolean): Int = {
    var data = Set(input: _*)
    var loop = true
    var count = 0
    while (loop) {
      var current: Option[(Int, Int)] = Some(start)
      var previous = current.get

      def useContainsOnSet(): Unit = {
        val (currentX, currentY) = current.get
        if (data.contains(current.get)) {
          current = Some((currentX - 1, currentY))

          if (data.contains(current.get)) {
            current = Some((currentX + 1, currentY))
            if (data.contains(current.get))
              current = None
            else
              previous = current.get
          } else {
            previous = current.get
          }
        } else {
          previous = current.get
          current = Some((currentX, currentY + 1))
        }
      }

      var broken = false
      while (current.isDefined) {
        val (_, currentY) = current.get
        if (break(currentY)) {
          broken = true
          current = None
        } else {
          useContainsOnSet()
        }
      }

      data += previous
      count += 1

      loop = continue(previous, broken)
    }

    count
  }

  println("answer1: " + (run(_ > yMax, (_, broken) => !broken) - 1))
  println("answer2: " + run(_ == yMax + 2, (previous, _) => previous != start))
}
