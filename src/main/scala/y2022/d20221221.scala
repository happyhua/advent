package advent
package y2022

import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221221(): Unit = {
  val puzzleInput = "2022/20221221e.txt" // 20221221.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  case class Operation(left: String, right: String, sign: String) {
    def reverse(original: String, name: String): String = {
      assert(left == name || right == name, this)

      if (left == name) {
        sign match {
          case "+" => original + " - " + right
          case "-" => original + " + " + right
          case "*" => original + " / " + right
          case _ => original + " * " + right
        }
      } else {
        sign match {
          case "+" => original + " - " + left
          case "-" => left + " - " + original
          case "*" => original + " / " + left
          case _ => left + " / " + original
        }
      }
    }
  }

  var input = lines.map(
    line =>
      val split = line.split(": ")
      split(0) -> split(1)
  ).toMap

  val regex = " ?([+\\-*\\/]) ?".r

  def getOperation(data: String): Operation = {
    val firstMatch = regex.findFirstMatchIn(data).get
    Operation(data.substring(0, firstMatch.start), data.substring(firstMatch.end), firstMatch.group(1))
  }

  def operation(data: String): Long = {
    val operation = getOperation(data)

    val sign = operation.sign
    val left = operation.left
    val right = operation.right

    sign match {
      case "+" => calculate(left) + calculate(right)
      case "-" => calculate(left) - calculate(right)
      case "*" => calculate(left) * calculate(right)
      case _ => calculate(left) / calculate(right)
    }
  }

  def calculate(monkeyNameOrNumber: String): Long = {
    monkeyNameOrNumber.toLongOption.getOrElse(
      {
        val data = input(monkeyNameOrNumber)
        data.toLongOption.getOrElse(operation(data))
      }
    )
  }

  val rootName = "root"

  println(calculate(rootName))

  val root = getOperation(input(rootName))
  val left = root.left
  val right = root.right

  val humnName = "humn"

  var s = humnName
  var variables = Seq(humnName)
  while (s != rootName) {
    val last = variables.last
    s = input.find(_._2.contains(last)).get._1
    variables = variables :+ s
  }

  val reference = if (variables.contains(left)) right else left

  assert(!variables.contains(reference))

  println(variables)

  variables.reduce(
    (l, r) =>
      input = input + (l -> getOperation(input(r)).reverse(r, l))

      r
  )

  input = input + (rootName -> (calculate(reference) << 1).toString)

  input.foreach(println(_))

  println(calculate(humnName))

  //  input = input.map(
  //    (monkey, data) =>
  //      if (skipped.contains(monkey))
  //        monkey -> {
  //          if (data.toLongOption.isDefined)
  //            data
  //          else {
  //            val operation = getOperation(data)
  //            val left = if (skipped.contains(operation.left)) operation.left else calculate(operation.left).toString
  //            val right = if (skipped.contains(operation.right)) operation.right else calculate(operation.right).toString
  //            left + " " + operation.sign + " " + right
  //          }
  //        }
  //      else
  //        monkey -> calculate(monkey).toString
  //  )

  //  val start = System.currentTimeMillis()
  //
  //  (1 to 0).foreach(
  //    i =>
  //      input = input.removed(humnName) + (humnName -> i.toString)
  //
  //      if (i % 100000 == 0)
  //        println(i + ", " + (System.currentTimeMillis() - start) / 1000 + " s")
  //
  //      if (calculate(left) == calculate(right)) {
  //        println(i)
  //      }
  //  )
}
