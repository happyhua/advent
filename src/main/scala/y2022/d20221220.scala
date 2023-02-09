package advent
package y2022

import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221220(): Unit = {
  val puzzleInput = "2022/20221220e.txt" // 20221220.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val input = lines.map(line => BigInt(line.toInt) * 811589153)

  println("input: " + input)

  val inputZip = input.zipWithIndex

  var result = inputZip
  (1 to 10).foreach(
    round =>
      var index = 0
      while (index < input.size) {
        val ((number, keyIndex), i) = result.zipWithIndex.find(_._1._2 == index).get

        // println(i + "\t" + number)
        val left = result.slice(0, i)
        val right = result.takeRight(result.size - i - 1)

        // println("left: " + left)
        // println("right: " + right)

        if (number == 0) {
          result = left :+ (number, keyIndex) :++ right
        } else if (number > 0) {
          val numberC = (number % (result.size - 1)).toInt
          if (numberC < right.size) {
            result = left :++ right.slice(0, numberC) :+ (number, keyIndex) :++ right.takeRight(right.size - numberC)
            //      } else if (numberC == right.size) {
            //        result = left :++ right :+ (number, true)
          } else {
            val correction = numberC - right.size

            result = left.slice(0, correction) :+ (number, keyIndex) :++ left.takeRight(left.size - correction) :++ right
          }
        } else {
          val numberC = (number % (result.size - 1)).toInt
          if (numberC.abs < left.size) {
            result = left.slice(0, left.size - numberC.abs) :+ (number, keyIndex) :++ left.takeRight(numberC.abs) :++ right
            //      } else if (numberC.abs == left.size) {
            //        result = Seq((number, true)) :++ left :++ right
          } else {
            val correction = numberC.abs - left.size

            result = left :++ right.slice(0, right.size - correction) :+ (number, keyIndex) :++ right.takeRight(correction)
          }
        }

        // println("debug: " + result)

        index += 1
      }

      println("round " + round + ": " + result.map(_._1))
  )

  // assert(result.find(!_._2).isEmpty)
  assert(result.size == input.size, result.size)
  println(result.map(_._1))

  val zero = result.zipWithIndex.find(_._1._1 == 0).get
  println(zero)

  val ordered = Seq(zero._1) :++ result.takeRight(result.size - zero._2 - 1) :++ result.slice(0, zero._2)
  println(ordered.map(_._1))

  val t1 = ordered(1000 % ordered.size)
  val t2 = ordered(2000 % ordered.size)
  val t3 = ordered(3000 % ordered.size)

  println(t1)
  println(t2)
  println(t3)

  println(t1._1 + t2._1 + t3._1)
}
