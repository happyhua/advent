package advent
package y2022

import scala.io.Source

@main
def d20221211(): Unit = {
  val puzzleInput = "2022/20221211e.txt" // 20221211.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  object Operation {
    def apply(operation: String): Operation = {
      val keyNew = "new = "

      assert(operation.startsWith("new = "))

      val split = operation.substring(keyNew.length).split(" [+*] ")

      assert(split.size == 2)
      assert(split(0) == "old", split(0))

      Operation(split(1).toLongOption, operation.contains("+"))
    }
  }

  case class Operation(variable: Option[Long] = None, plus: Boolean = true) {
    def operate(item: Long): Long = {
      val value = variable.getOrElse(item)

      if (plus) item + value else item * value
    }
  }

  case class Monkey(var items: Seq[Long], operation: Operation, divisor: Int, ifTrue: Int, ifFalse: Int) {
    private val initItems = items
    private var inspectedItemsSize = 0

    def run(monkeys: Seq[Monkey], postOperate: Long => Long): Unit = {
      inspectedItemsSize += items.size

      val update = items.map(
        item =>
          val newItem = postOperate(operation.operate(item))

          val monkeyIndex = if (newItem % divisor == 0) ifTrue else ifFalse

          (monkeyIndex, newItem)
      ).groupMap(_._1)(_._2)

      update.foreach(
        (monkeyIndex, newItems) =>
          monkeys(monkeyIndex).items = monkeys(monkeyIndex).items :++ newItems
      )

      items = Seq()
    }

    def reset(): Unit = {
      items = initItems
      inspectedItemsSize = 0
    }

    def getInspectedItemsSize: Int = inspectedItemsSize
  }

  val regexStartingItems = "Starting items: (.*)".r
  val comma = ", "
  val regexOperation = "Operation: (.*)".r
  val regexTestDivisor = "Test: divisible by ([0-9]+)".r
  val regexIfTrue = "If true: throw to monkey ([0-9]+)".r
  val regexIfFalse = "If false: throw to monkey ([0-9]+)".r

  val monkeys = lines.grouped(7).map(
    groupedLines =>
      val items = regexStartingItems.findFirstMatchIn(groupedLines(1)).get.group(1).split(comma).map(_.toLong).toSeq
      val operation = Operation(regexOperation.findFirstMatchIn(groupedLines(2)).get.group(1))
      val divisor = regexTestDivisor.findFirstMatchIn(groupedLines(3)).get.group(1).toInt
      val ifTrue = regexIfTrue.findFirstMatchIn(groupedLines(4)).get.group(1).toInt
      val ifFalse = regexIfFalse.findFirstMatchIn(groupedLines(5)).get.group(1).toInt

      Monkey(items, operation, divisor, ifTrue, ifFalse)
  ).toSeq

  // monkeys.foreach(println(_))

  def getAnswer(rounds: Int, postOperate: Long => Long): Long = {
    (1 to rounds).foreach(_ => monkeys.foreach(_.run(monkeys, postOperate)))

    val answer = monkeys.map(_.getInspectedItemsSize).sorted.takeRight(2)

    answer.head.toLong * answer(1)
  }

  println("answer1: " + getAnswer(20, item => item / 3))

  monkeys.foreach(_.reset())
  val divisorProduct = monkeys.map(_.divisor).product
  println("answer2: " + getAnswer(10000, item => item % divisorProduct))
}
