package advent
package y2022

import scala.io.Source

@main
def d20221213(): Unit = {
  val puzzleInput = "2022/20221213e.txt" // 20221213.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  object Node {
    def apply(line: String): Node = {
      var buffer = ""
      line.substring(1).foldLeft(Node())(
        (_node, char) =>
          if (char == '[') {
            _node.appendChild(Node())
          } else if (char == ']') {
            if (buffer.nonEmpty) {
              _node.appendChild(Node(buffer.toIntOption))
              buffer = ""
            }
            if (_node.parent == null) _node else _node.parent
          } else {
            if (char == ',') {
              if (buffer.nonEmpty) {
                _node.appendChild(Node(buffer.toIntOption))
                buffer = ""
              }
            } else {
              buffer += char
            }
            _node
          }
      )
    }
  }

  case class Node(value: Option[Int] = None, var children: Seq[Node] = Seq()) extends Comparable[Node] {
    var parent: Node = _

    def appendChild(child: Node): Node = {
      child.parent = this

      children = children :+ child

      child
    }

    private def compare(children: Seq[Node], otherChildren: Seq[Node]): Int = {
      var result = 0

      children.zip(otherChildren).takeWhile(
        (node, other) =>
          result = node.compareTo(other)

          result == 0
      )

      if (result == 0)
        children.size.compareTo(otherChildren.size)
      else
        result
    }

    override def compareTo(other: Node): Int = {
      if (value.isDefined && other.value.isEmpty) {
        compare(Seq(Node(value)), other.children)
      } else if (value.isEmpty && other.value.isDefined) {
        compare(children, Seq(Node(other.value)))
      } else if (value.isDefined && other.value.isDefined) {
        val result = value.get.compare(other.value.get)

        if (result == 0)
          compare(children, other.children)
        else
          result
      } else {
        compare(children, other.children)
      }
    }

    override def toString: String = {
      if (value.isDefined)
        value.get.toString
      else
        children.map(_.toString).mkString("[", ",", "]")
    }
  }

  val pairs = lines.grouped(3).map(_.take(2).map(Node(_))).toSeq

  val pairsInRightOrder = pairs.map(
    pair =>
      pair.head.compareTo(pair(1))
  ).zipWithIndex.filter(_._1 < 0).map(_._2 + 1)

  println("answer1: " + pairsInRightOrder.sum)

  val divider1 = Node("[[2]]")
  val divider2 = Node("[[6]]")
  val pairsSorted = (pairs.flatten :++ Seq(divider1, divider2)).sorted

  // pairsSorted.foreach(println(_))

  println("answer2: " + (pairsSorted.indexOf(divider1) + 1) * (pairsSorted.indexOf(divider2) + 1))
}
