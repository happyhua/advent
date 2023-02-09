package advent
package y2022

import scala.io.Source

@main
def d20221216(): Unit = {
  val puzzleInput = "2022/20221216e.txt" // 20221216.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  def openValve(valve: String, open: Map[String, Int], minute: Int)(implicit valves: Map[String, (Int, Seq[String])])
  : Option[(String, Map[String, Int])] = {
    if (valves(valve)._1 > 0 && !open.contains(valve))
      Some((valve, open + (valve -> minute)))
    else
      None
  }

  def sum(open: Map[String, Int])(implicit valves: Map[String, (Int, Seq[String])], minutes: Int) = {
    open.map((valve, minute) => valves(valve)._1 * (minutes - minute)).sum
  }

  val valveKey = "Valve "

  implicit val valves: Map[String, (Int, Seq[String])] = lines.map(
    line =>
      val split = line.split("; tunnels? leads? to valves? ")

      val splitValve = split(0).split('=')

      val name = splitValve(0).substring(valveKey.length, valveKey.length + 2)

      (name, (splitValve(1).toInt, split(1).split(", ").toSeq))
  ).toMap

  val start = "AA"
  /* A performance tuning variable. Small number can make a wrong output, large number makes processing time longer */
  val pathsMax = 10000

  implicit var minutes: Int = 30
  var minute = 0
  var paths1: Seq[(String, Map[String, Int])] = Seq((start, Map()))
  while (minute < minutes) {
    minute += 1
    paths1 = paths1.flatMap(
      (valve, open) =>
        openValve(valve, open, minute) ++ valves(valve)._2.map((_, open))
    ).distinct

    paths1 = paths1.sortBy((_, open) => sum(open)).takeRight(paths1.size.min(pathsMax))
  }
  println("answer1: " + paths1.map((_, open) => sum(open)).max)

  minutes = 26
  minute = 0
  var paths2: Seq[(String, String, Map[String, Int])] = Seq((start, start, Map()))
  while (minute < minutes) {
    minute += 1
    paths2 = paths2.flatMap(
      (valveMe, valveElephant, open) =>
        val valvesMeNew = openValve(valveMe, open, minute) ++ valves(valveMe)._2.map((_, open))
        val valvesElephantNew = openValve(valveElephant, open, minute) ++ valves(valveElephant)._2.map((_, open))

        valvesMeNew.flatMap(
          (valveMeNew, open) =>
            valvesElephantNew.map(
              (valveElephantNew, openElephant) =>
                (valveMeNew, valveElephantNew, open ++ openElephant)
            )
        )
    ).distinct

    paths2 = paths2.sortBy((_, _, open) => sum(open)).takeRight(paths2.size.min(pathsMax))
  }
  println("answer2: " + paths2.map((_, _, open) => sum(open)).max)
}
