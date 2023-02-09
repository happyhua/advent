package advent
package y2022

import scala.io.Source

// TODO, code is a snapshot during assignment, needs to be improved
@main
def d20221219(): Unit = {
  val puzzleInput = "2022/20221219e.txt" // 20221219.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  val regex = ("Blueprint ([0-9]{1,2}): Each ore robot costs ([0-9]{1,2}) ore. Each clay robot costs ([0-9]{1,2}) ore. "
    + "Each obsidian robot costs ([0-9]{1,2}) ore and ([0-9]{1,2}) clay. Each geode robot costs ([0-9]{1,2}) ore and "
    + "([0-9]{1,2}) obsidian.").r

  def createBlueprint(line: String): Blueprint = {
    val firstMatch = regex.findFirstMatchIn(line).get

    Blueprint(firstMatch.group(1).toInt, OreRobot(firstMatch.group(2).toInt), ClayRobot(firstMatch.group(3).toInt),
      ObsidianRobot(firstMatch.group(4).toInt, firstMatch.group(5).toInt), GeodeRobot(firstMatch.group(6).toInt,
        firstMatch.group(7).toInt))
  }

  val blueprints = lines.map(createBlueprint(_))

  println("answer: " + blueprints.take(3).map(blueprint => blueprint.run()))

  //  println(index)
}

enum Mineral {
  case Ore, Clay, Obsidian, Geode
}

case class OreRobot(ore: Int) {
  def canBuild(minerals: Minerals): Boolean = minerals.ore >= ore && minerals.ore < (ore << 1)

  def build(minerals: Minerals): Minerals = minerals.copy(ore = minerals.ore - ore)
}

case class ClayRobot(ore: Int) {
  def canBuild(minerals: Minerals): Boolean = minerals.ore >= ore && minerals.ore < (ore << 1)

  def build(minerals: Minerals): Minerals = minerals.copy(ore = minerals.ore - ore)
}

case class ObsidianRobot(ore: Int, clay: Int) {
  def canBuild(minerals: Minerals): Boolean = minerals.ore >= ore && minerals.clay >= clay

  def build(minerals: Minerals): Minerals = minerals.copy(ore = minerals.ore - ore, clay = minerals.clay - clay)
}

case class GeodeRobot(ore: Int, obsidian: Int) {
  def canBuild(minerals: Minerals): Boolean = minerals.ore >= ore && minerals.obsidian >= obsidian

  def build(minerals: Minerals): Minerals = minerals.copy(ore = minerals.ore - ore, obsidian = minerals.obsidian -
    obsidian)
}

case class Robots(var oreRobot: Int = 0, var clayRobot: Int = 0, var obsidianRobot: Int = 0, var geodeRobot: Int = 0,
                  var extra: Robots = null) {
  def lazyAdd(oreRobot: Int = 0, clayRobot: Int = 0, obsidianRobot: Int = 0, geodeRobot: Int = 0): Robots = {
    if (extra == null)
      extra = Robots()

    extra.oreRobot += oreRobot
    extra.clayRobot += clayRobot
    extra.obsidianRobot += obsidianRobot
    extra.geodeRobot += geodeRobot

    this
  }

  def fill(minerals: Minerals): Unit = {
    minerals.ore += oreRobot
    minerals.clay += clayRobot
    minerals.obsidian += obsidianRobot
    minerals.geode += geodeRobot

    if (extra != null) {
      oreRobot += extra.oreRobot
      clayRobot += extra.clayRobot
      obsidianRobot += extra.obsidianRobot
      geodeRobot += extra.geodeRobot

      extra = null
    }
  }
}

case class Minerals(var ore: Int = 0, var clay: Int = 0, var obsidian: Int = 0, var geode: Int = 0)

case class Blueprint(id: Int, oreRobot: OreRobot, clayRobot: ClayRobot, obsidianRobot: ObsidianRobot,
                     geodeRobot: GeodeRobot) {
  var mineralsSet = Set((Robots(oreRobot = 1), Minerals()))

  val minutes = 32

  def value(robots: Robots, minerals: Minerals, index: Int): Int = {
    val copy = minerals.copy()

    val remaining = minutes - index

    copy.ore += robots.oreRobot * remaining
    copy.clay += robots.clayRobot * remaining
    copy.obsidian += robots.obsidianRobot * remaining
    copy.geode += robots.geodeRobot * remaining

    var orc = copy.ore
    orc += copy.clay * clayRobot.ore
    orc += copy.obsidian * (obsidianRobot.ore + obsidianRobot.clay * clayRobot.ore)
    orc += copy.geode * (geodeRobot.ore + geodeRobot.obsidian * (obsidianRobot.ore + obsidianRobot.clay * clayRobot.ore))

    orc
  }

  def run(): Int = {
    var index = 0
    while (index < minutes - 1) {
      mineralsSet = mineralsSet.flatMap(
        (robots, minerals) =>
          var result = Set((robots, minerals))

          if (oreRobot.canBuild(minerals))
            result += (robots.copy().lazyAdd(oreRobot = 1), oreRobot.build(minerals))

          if (clayRobot.canBuild(minerals))
            result += (robots.copy().lazyAdd(clayRobot = 1), clayRobot.build(minerals))

          if (obsidianRobot.canBuild(minerals))
            result += (robots.copy().lazyAdd(obsidianRobot = 1), obsidianRobot.build(minerals))

          if (geodeRobot.canBuild(minerals))
            result = Set((robots.copy().lazyAdd(geodeRobot = 1), geodeRobot.build(minerals)))

          result.foreach(
            (robots, minerals) => robots.fill(minerals)
          )

          result

        //                (robots, minerals) =>
        //                  if (oreRobot.canBuild(minerals))
        //                    Seq((robots.copy().lazyAdd(oreRobot = 1), oreRobot.build(minerals)), (robots, minerals))
        //                  else
        //                    Seq((robots, minerals))
        //              ).flatMap(
        //                (robots, minerals) =>
        //                  if (clayRobot.canBuild(minerals))
        //                    Seq((robots.copy().lazyAdd(clayRobot = 1), clayRobot.build(minerals)), (robots, minerals))
        //                  else
        //                    Seq((robots, minerals))
        //              ).flatMap(
        //                (robots, minerals) =>
        //                  if (obsidianRobot.canBuild(minerals))
        //                    Seq((robots.copy().lazyAdd(obsidianRobot = 1), obsidianRobot.build(minerals)), (robots, minerals))
        //                  else
        //                    Seq((robots, minerals))
        //              ).flatMap(
        //                (robots, minerals) =>
        //                  if (geodeRobot.canBuild(minerals))
        //                    Seq((robots.copy().lazyAdd(geodeRobot = 1), geodeRobot.build(minerals)))
        //                  else
        //                    Seq((robots, minerals))
        //              ).map(
        //                (robots, minerals) =>
        //                  robots.fill(minerals)
        //                  (robots, minerals)
      )

      // mineralsSeq = mineralsSeq.sortBy((robots, minerals) => -value(robots, minerals, index)).take(100000)

      index += 1

      //      var max = mineralsSet.map((_, minerals) => minerals.geode).max
      //
      //      if (max > 0)
      //        mineralsSet.filter((_, minerals) => minerals.geode == max).foreach(println(_))

      println(index + "\t" + mineralsSet.size)

      // mineralsSet.foreach((robots, minerals) => println(robots.toString + ", " + robots.extra + ", " + minerals))
      // mineralsSet.foreach((robots, minerals) => println(value(robots, minerals, index)))

      //      mineralsSeq = mineralsSeq.groupMap(_._1)(_._2).toSeq.flatMap(
      //        (robots, minerals) =>
      //          minerals.groupBy(_.geode).flatMap(
      //            _._2.groupBy(_.obsidian).flatMap(
      //              _._2.groupBy(_.clay).flatMap(
      //                minerals =>
      //                  val max = minerals._2.map(_.ore).max
      //                  minerals._2.filter(_.ore == max))
      //            )
      //          ).groupBy(_.geode).flatMap(
      //            _._2.groupBy(_.obsidian).flatMap(
      //              _._2.groupBy(_.ore).flatMap(
      //                minerals =>
      //                  val max = minerals._2.map(_.clay).max
      //                  minerals._2.filter(_.clay == max))
      //            )
      //          ).map((robots, _))
      //      )

      // minerals.map((robots, _))
    }

    val result = mineralsSet.map(
      (robots, minerals) =>
        //        var extra = 0
        //        val _robots = robots.copy()
        //        var _minerals = minerals.copy()
        //        _robots.fill(_minerals)
        //        while (geodeRobot.canBuild(_minerals, _robots.oreRobot, _robots.obsidianRobot)) {
        //          _minerals = geodeRobot.build(_minerals)
        //
        //          extra += 1
        //        }
        //
        //        if (robots.extra != null && robots.extra.geodeRobot > 0)
        //          extra += (robots.extra.geodeRobot << 0)
        //
        //        (robots.geodeRobot << 0) + minerals.geode + extra
        robots.fill(minerals)
        minerals.geode
    ).max

    // println(mineralsSeq.map((robots, minerals) => robots.geodeRobot + minerals.geode).max)

    println(id + ": " + result)

    result
  }
}
