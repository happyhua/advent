package advent
package y2022

import scala.io.Source

@main
def d20221225(): Unit = {
  val puzzleInput = "2022/20221225e.txt" // 20221225.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val divisor = 5

  def snafu2Decimal(snafu: String): Long = {
    snafu.reverse.zipWithIndex.map(
      (char, index) =>
        val pow5 = math.pow(divisor, index).asInstanceOf[Long]

        val base =
          char match
            case '2' => 2
            case '1' => 1
            case '0' => 0
            case '-' => -1
            case '=' => -2

        pow5 * base
    ).sum
  }

  def decimal2Snafu(decimal: Long): String = {
    var borrow = 0
    val snafu = java.lang.Long.toString(decimal, divisor).reverse.map(
      char =>
        val dividend = borrow + char.asDigit

        val remainder = dividend % divisor
        borrow = dividend / divisor

        if (remainder < 3)
          Character.forDigit(remainder, divisor)
        else
          borrow += 1

          if (remainder == 3)
            '='
          else
            '-'
    ).reverse

    if (borrow > 0)
      borrow + snafu
    else
      snafu
  }

  val decimals = lines.map(snafu2Decimal(_))
  // decimals.foreach(println(_))

  val decimalSum = decimals.sum
  println("decimal sum: " + decimalSum)

  val snafu = decimal2Snafu(decimalSum)
  assert(decimalSum == snafu2Decimal(snafu))
  println("snafu: " + snafu)
}
