package advent
package y2022

import scala.collection.mutable
import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221218(): Unit = {
  val puzzleInput = "2022/20221218e.txt" // 20221218.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val input = lines.map(
    line =>
      val split = line.split(",")
      (split(0).toInt, split(1).toInt, split(2).toInt)
  )

  def isBlocked(x1: Int, y1: Int, z1: Int): (Int, Int, Int) => Boolean = {
    (x2, y2, z2) =>
      (x1 - x2).abs == 1 && y1 == y2 && z1 == z2
        || x1 == x2 && (y1 - y2).abs == 1 && z1 == z2
        || x1 == x2 && y1 == y2 && (z1 - z2).abs == 1
  }

  val answer = input.map(
    (x1, y1, z1) =>
      6 - input.count(isBlocked(x1, y1, z1)(_, _, _))
  )

  //  input.foreach(
  //    (x1, y1, z1) =>
  //      println(x1 + "\t" + y1 + "\t" + z1)
  //      val filter = input.filter(
  //        (x2, y2, z2) =>
  //          (x1 - x2).abs == 1 && y1 == y2 && z1 == z2
  //            || x1 == x2 && (y1 - y2).abs == 1 && z1 == z2
  //            || x1 == x2 && y1 == y2 && (z1 - z2).abs == 1
  //      )
  //      println(filter.size)
  //      filter.foreach(println(_))
  //  )

  // answer.foreach(println(_))
  println("sum: " + answer.sum)

  val xSeq = input.map(_._1)
  val ySeq = input.map(_._2)
  val zSeq = input.map(_._3)

  val xMin = xSeq.min
  val xMax = xSeq.max
  val yMin = ySeq.min
  val yMax = ySeq.max
  val zMin = zSeq.min
  val zMax = zSeq.max

  println(xMin + "\t" + xMax + "\t" + yMin + "\t" + yMax + "\t" + zMin + "\t" + zMax)
  val map = (xMin to xMax).flatMap(
    x => (yMin to yMax).flatMap(
      y => (zMin to zMax).map(
        z => (x, y, z)
      )
    )
  ).diff(input)

  def isBlockedInInput(x1: Int, y1: Int, z1: Int): Boolean = {
    input.exists((x2, y2, z2) => x2 < x1 && y1 == y2 && z1 == z2)
      && input.exists((x2, y2, z2) => x2 > x1 && y1 == y2 && z1 == z2)
      && input.exists((x2, y2, z2) => x1 == x2 && y2 < y1 && z1 == z2)
      && input.exists((x2, y2, z2) => x1 == x2 && y2 > y1 && z1 == z2)
      && input.exists((x2, y2, z2) => x1 == x2 && y1 == y2 && z2 < z1)
      && input.exists((x2, y2, z2) => x1 == x2 && y1 == y2 && z2 > z1)
  }

  val filter = map.filter(isBlockedInInput(_, _, _))

  println("filter: " + filter.size)

  val inputSet = input.toSet
  val filterSet = filter.toSet

  def xNeighbours(x: Int, y: Int, z: Int): Seq[(Int, Int, Int)] = {
    var result: Seq[(Int, Int, Int)] = Seq()
    var dx = -1
    while (!inputSet.contains(x + dx, y, z)) {
      dx += -1

      result = result :+ (x + dx, y, z)
    }

    dx = 1
    while (!inputSet.contains(x + dx, y, z)) {
      dx += 1

      result = result :+ (x + dx, y, z)
    }

    result
  }

  def neighbours(x: Int, y: Int, z: Int): Seq[(Int, Int, Int)] = {
    assert(!inputSet.contains((x, y, z)))

    xNeighbours(x, y, z)
  }

  var unblockedSet: Set[(Int, Int, Int)] = Set()

  def anyUnblockedNeighbours(x: Int, y: Int, z: Int, visisted: mutable.Set[(Int, Int, Int)]): Boolean = {
    if (visisted.contains(x, y, z))
      return false

    visisted += ((x, y, z))

    val check = Seq(
      (x - 1, y, z),
      (x + 1, y, z),
      (x, y - 1, z),
      (x, y + 1, z),
      (x, y, z - 1),
      (x, y, z + 1)
    ).filterNot(t => visisted.contains(t) || inputSet.contains(t))
    if (check.exists((x, y, z) => x < xMin || x > xMax || y < yMin || y > yMax || z < zMin || z > zMax))
      true
    else if (check.isEmpty)
      false
    else {
      check.exists(
        (x1, y1, z1) =>
          anyUnblockedNeighbours(x1, y1, z1, visisted)
      )
    }
  }

  var reducedFilter = filter.flatMap(
    (x1, y1, z1) =>
      if (unblockedSet.contains((x1, y1, z1)))
        None
      else {
        val visisted = mutable.Set[(Int, Int, Int)]()

        if (anyUnblockedNeighbours(x1, y1, z1, visisted)) {
          unblockedSet ++= visisted

          None
        } else {
          Some(x1, y1, z1)
        }
      }
  )

  println("reducedFilter: " + reducedFilter.size)

  val all = input :++ reducedFilter

  val answer2 = all.map(
    (x1, y1, z1) =>
      (x1, y1, z1, 6 - all.count((x2, y2, z2) => isBlocked(x1, y1, z1)(x2, y2, z2)))
  )

  println(answer2.map(_._4).sum)

  val filterSum = reducedFilter.map(
    (x1, y1, z1) =>
      6 - reducedFilter.count((x2, y2, z2) => isBlocked(x1, y1, z1)(x2, y2, z2))
  ).sum

  println("reducedFilterSum: " + filterSum)

  val answer3 = input.map(
    (x1, y1, z1) =>
      (x1, y1, z1, 6 - all.count((x2, y2, z2) => isBlocked(x1, y1, z1)(x2, y2, z2)))
  )

  println(answer3.map(t => t._4).sum)
  println(answer.sum - filterSum)

  // answer2.diff(answer3).filter(_._4 > 0).foreach(println(_))

  //  println(map.diff(input).size)
  //
  //  // map.foreach(println(_))
  //  val filter = map.diff(input).filter(
  //    (x1, y1, z1) =>
  //      input.count(
  //        (x2, y2, z2) =>
  //          (x1 - x2).abs == 1 && y1 == y2 && z1 == z2
  //            || x1 == x2 && (y1 - y2).abs == 1 && z1 == z2
  //            || x1 == x2 && y1 == y2 && (z1 - z2).abs == 1
  //      ) == 6
  //  )
  //
  //  println("filter: " + filter.size)
  //
  //  val filterDiff = input :++ filter // .diff(input)
  //
  //  println(input.size + "\t" + filterDiff.size)
  //
  //
  //  val filterSum = input.map(
  //    (x1, y1, z1) =>
  //      6 - filterDiff.count(
  //        (x2, y2, z2) =>
  //          (x1 - x2).abs == 1 && y1 == y2 && z1 == z2
  //            || x1 == x2 && (y1 - y2).abs == 1 && z1 == z2
  //            || x1 == x2 && y1 == y2 && (z1 - z2).abs == 1
  //      )
  //  ).sum
  //
  //  println(filterSum)

  //  println(answer.sum - filterDiff.size * 6)
  //  println(answer.sum - filterSum)
}
