package advent
package y2022

import util.*

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.io.Source

// TODO, code needs to be further improved
@main
def d20221217(): Unit = {
  val puzzleInput = "2022/20221217e.txt" // 20221217.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  case class Coordinate(var x: Int, var y: Int)

  object Jet {
    private var jet: String = _
    var index = 0

    def next(): Boolean = {
      val isRight = jet(index) == '>'

      index = (index + 1) % jet.length

      isRight
    }

    def load(jet: String): Unit = {
      Jet.jet = jet
      index = 0
    }
  }

  object Chamber {
    private val size = 3
    private val columnSize = 7
    private val columnMaxIndex = columnSize - 1

    val rows: mutable.Buffer[Array[Boolean]] = mutable.Buffer()

    private def addRows(adjustment: Int) = rows ++= LazyList.fill(adjustment)(new Array[Boolean](columnSize))

    def tall(): Int = rows.size - rows.takeRight(size).count(_.forall(column => !column))

    def nextRock(rock: Rock): Coordinate = {
      val required = size // + rock.height

      val adjustment = required - rows.takeRight(size).count(_.forall(column => !column))

      if (adjustment > 0)
        addRows(adjustment)
      else if (adjustment < 0)
        rows.dropRightInPlace(adjustment.abs)

      // assert(rows.count(_.forall(column => !column)) == required, required)

      Coordinate(2, rows.size)
    }

    private def isToLeftBlocked(x: Int, y: Int): Boolean = {
      if (x - 1 < 0)
        true
      else if (y < rows.size)
        rows(y)(x - 1)
      else
        false
    }

    def left(rock: Rock, coordinate: Coordinate): Unit = {
      if (!rock.left.exists(point => isToLeftBlocked(coordinate.x + point._1, coordinate.y + point._2)))
        coordinate.x -= 1
    }

    private def isToRightBlocked(x: Int, y: Int): Boolean = {
      if (x + 1 > columnMaxIndex)
        true
      else if (y < rows.size)
        rows(y)(x + 1)
      else
        false
    }

    def right(rock: Rock, coordinate: Coordinate): Unit = {
      if (!rock.right.exists(point => isToRightBlocked(coordinate.x + point._1, coordinate.y + point._2)))
        coordinate.x += 1
    }

    private def isToDownBlocked(x: Int, y: Int): Boolean = {
      if (y - 1 < 0)
        true
      else if (y - 1 < rows.size)
        rows(y - 1)(x)
      else
        false
    }

    def down(rock: Rock, coordinate: Coordinate): Boolean = {
      val blocked = rock.down.exists(point => isToDownBlocked(coordinate.x + point._1, coordinate.y + point._2))

      if (blocked) {
        rock.all.foreach(
          point =>
            val y = coordinate.y + point._2

            if (y >= rows.size)
              addRows(y - rows.size + 1)

            rows(y)(coordinate.x + point._1) = true
        )
      }
      else {
        coordinate.y -= 1
      }

      blocked
    }

    def printChamber(): Unit = {
      rows.reverse.foreach(
        row =>
          row.foreach(cell => print(if (cell) '#' else '.'))
          println()
      )
    }
  }

  def run(numberRocks: Long, handler: Int => Unit = _ => ()): Unit = {
    reset()

    var index = 0

    while (index < numberRocks) {
      handler.apply(index)

      index += 1

      val rock = Rock.next()
      var jet = true
      val coordinate = Chamber.nextRock(rock)

      var blocked = false
      while (!blocked) {
        if (jet) {
          if (Jet.next())
            Chamber.right(rock, coordinate)
          else
            Chamber.left(rock, coordinate)
        } else {
          blocked = Chamber.down(rock, coordinate)
        }

        jet = !jet
      }
    }
  }

  def reset(): Unit = {
    Chamber.rows.clear()
    Jet.index = 0
    Rock.index = 0
  }

  Jet.load(lines.head)

  val star1 = 2022
  val star2 = 1000000000000L

  var star = star2

  val sample = 10000
  var sampleSeq: Seq[Int] = Seq()
  run(sample, _ => sampleSeq :+= Chamber.tall())
  sampleSeq = sampleSeq.zip(sampleSeq.tail).map(line => line._2 - line._1).tail
  val pattern = repeatedExact(sampleSeq.reverse)
  println(pattern.size)
  run(star % pattern.size)
  println("answer2: " + (star / pattern.size * pattern.sum + Chamber.tall()))
}

object Rock {
  var index = 0

  def next(): Rock = {
    val rock = Rock.values(index)

    index = (index + 1) % Rock.values.length

    rock
  }
}

enum Rock(val right: Seq[(Int, Int)], val left: Seq[(Int, Int)], val down: Seq[(Int, Int)]) {
  case H extends Rock(Seq((3, 0)), Seq((0, 0)), Seq((0, 0), (1, 0), (2, 0), (3, 0)))
  case P extends Rock(Seq((1, 0), (2, 1), (1, 2)), Seq((1, 0), (0, 1), (1, 2)), Seq((0, 1), (1, 0), (2, 1)))
  case L extends Rock(Seq((2, 0), (2, 1), (2, 2)), Seq((0, 0)), Seq((0, 0), (1, 0), (2, 0)))
  case V extends Rock(Seq((0, 0), (0, 1), (0, 2), (0, 3)), Seq((0, 0), (0, 1), (0, 2), (0, 3)), Seq((0, 0)))
  case S extends Rock(Seq((1, 0), (1, 1)), Seq((0, 0), (0, 1)), Seq((0, 0), (1, 0)))

  val all: Seq[(Int, Int)] = (right :++ left :++ down).distinct

  val height: Int =
    val ys = all.map(_._2)
    ys.max - ys.min + 1
}
