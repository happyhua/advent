package advent
package y2022

import scala.io.Source

@main
def d20221215(): Unit = {
  val puzzleInput = "2022/20221215e.txt" // 20221215.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  case class Sensor(x: Int, y: Int) {
    private def getDistance(beacon: (Int, Int)): Int = {
      (x - beacon._1).abs + (y - beacon._2).abs
    }

    def xRange(beacon: (Int, Int), targetY: Int): Option[(Int, Int)] = {
      val distance = getDistance(beacon)

      val distanceX = distance - (y - targetY).abs

      if (distanceX < 0)
        None
      else
        Some((x - distanceX, x + distanceX))
    }
  }

  val (targetY, maxY) = if (puzzleInput.contains("e")) (10, 20) else (2000000, 4000000)

  val input = lines.map(
    line =>
      val split = line.split(":")
      val splitSensor = split(0).split(",")
      val splitBeacon = split(1).split(",")
      (Sensor(splitSensor(0).split("=")(1).toInt, splitSensor(1).split("=")(1).toInt),
        (splitBeacon(0).split("=")(1).toInt, splitBeacon(1).split("=")(1).toInt))
  )

  def getXRanges(targetY: Int): Seq[(Int, Int)] = {
    input.flatMap(
      pair => pair._1.xRange(pair._2, targetY)
    ).sortBy(_._1)
  }

  val xRanges = getXRanges(targetY)
  var holes = 0
  val xRange = xRanges.reduce(
    (left, right) =>
      if (right._1 > left._2 + 1)
        holes += right._1 - (left._2 + 1)

      (left._1, left._2.max(right._2))
  )
  println("answer1: " + (xRange._2 - xRange._1 - holes + 1 - input.map(_._2).distinct.count(_._2 == targetY)))

  (0 to maxY).takeWhile(
    y =>
      var continue = true

      val xRanges = getXRanges(y)
      xRanges.reduce(
        (left, right) =>
          if (right._1 > left._2 + 1) {
            println("answer2: " + ((left._2 + 1) * 4000000L + y))
            continue = false
          }

          (left._1, left._2.max(right._2))
      )

      continue
  )
}
