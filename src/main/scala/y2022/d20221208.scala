package advent
package y2022

import scala.io.Source

@main
def d20221208(): Unit = {
  val puzzleInput = "2022/20221208e.txt" // 20221208.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val rows = lines.map(_.map(_.asDigit))
  val columns = rows.transpose

  val answer1 = rows.zipWithIndex.map(
    (row, rowIndex) =>
      row.zipWithIndex.filter(
        (height, columnIndex) =>
          val column = columns(columnIndex)

          height > row.take(columnIndex).maxOption.getOrElse(-1)
            || height > row.takeRight(row.length - 1 - columnIndex).maxOption.getOrElse(-1)
            || height > column.take(rowIndex).maxOption.getOrElse(-1)
            || height > column.takeRight(column.length - 1 - rowIndex).maxOption.getOrElse(-1)
      )
  )

  println("answer1: " + answer1.map(_.size).sum)

  def viewingDistance(height: Int, trees: Seq[Int]) = {
    trees.length.min(trees.takeWhile(height > _).size + 1)
  }

  val answer2 = rows.zipWithIndex.map(
    (row, rowIndex) =>
      row.zipWithIndex.map(
        (height, columnIndex) =>
          Seq(
            viewingDistance(height, row.take(columnIndex).reverse)
            , viewingDistance(height, row.takeRight(row.length - 1 - columnIndex))
            , viewingDistance(height, columns(columnIndex).take(rowIndex).reverse)
            , viewingDistance(height, columns(columnIndex).takeRight(row.length - 1 - rowIndex))
          )
      )
  )

  println("answer2: " + answer2.map(_.map(_.product).max).max)
}
