package advent

package object util {
  type printer = (Int, Int, Boolean) => Unit

  private val defaultPrinter: printer = (row, column, present) => print((row, column, present).toString())

  def printData(data: Iterable[(Int, Int)], printer: printer = defaultPrinter, autoNewLine: Boolean = true,
                rowRangeOption: Option[(Int, Int)] = None, columnRangeOption: Option[(Int, Int)] = None): Unit = {
    val dataSet = data.toSet

    val rowRange = rowRangeOption.getOrElse(
      {
        val rows = dataSet.map(_._1)
        (rows.min, rows.max)
      }
    )

    val columnRange = columnRangeOption.getOrElse(
      {
        val columns = dataSet.map(_._2)
        (columns.min, columns.max)
      }
    )

    println(rowRange.toString() + ", " + columnRange)

    (rowRange(0) to rowRange(1)).foreach(
      rowIndex =>
        (columnRange(0) to columnRange(1)).foreach(
          columnIndex => printer(rowIndex, columnIndex, dataSet.contains(rowIndex, columnIndex)))

        if (autoNewLine)
          println()
    )
  }

  def repeatedExactWithOffsetNormalized[A](input: Seq[A]): Seq[A] = {
    var pattern = repeatedExactWithOffset(input)

    if (pattern.nonEmpty) {
      var rotate = input.take(input.indexOfSlice(pattern))

      while (pattern.last == rotate.last) {
        pattern = pattern.init.prepended(pattern.last)
        rotate = rotate.init
      }
    }

    pattern
  }

  def repeatedExactWithOffset[A](input: Seq[A]): Seq[A] = {
    repeatedExact(input.reverse).reverse
  }

  def repeatedExact[A](input: Seq[A]): Seq[A] = {
    repeatedExact(input, repeatedContains(input))
  }

  def repeatedContains[A](input: Seq[A]): Seq[A] = {
    var containsPattern = Seq(input.head)
    var tail = input.tail

    while (tail.nonEmpty) {
      if (tail.containsSlice(containsPattern)) {
        containsPattern = containsPattern :+ tail.head
        tail = tail.tail
      } else {
        tail = Seq()
      }
    }

    containsPattern.init
  }

  def repeatedExact[A](input: Seq[A], containsPattern: Seq[A]): Seq[A] = {
    var exactPattern = containsPattern

    var loop = true
    while (exactPattern.nonEmpty && loop) {
      if (input.slice(exactPattern.size, exactPattern.size << 1) == exactPattern) {
        loop = false
      } else {
        exactPattern = exactPattern.init
      }
    }

    exactPattern
  }
}
