package advent
package y2022

import scala.io.Source
import scala.runtime.stdLibPatches.Predef.assert

@main
def d20221207(): Unit = {
  val puzzleInput = "2022/20221207e.txt" // 20221207.txt is my real puzzle input data

  val lines = Source.fromResource(puzzleInput).getLines().toSeq

  case class File(name: String, size: Option[Int] = None, parent: File = null) {
    private var children = Seq[File]()

    def getChild(name: String): File = children.find(_.name == name).get

    def appendChild(name: String, size: Option[Int] = None): File = {
      assert(this.size.isEmpty && !children.exists(_.name == name), toString + ": " + name)

      children = children :+ File(name, size, this)

      this
    }

    private def listFiles(): Seq[File] = {
      children.flatMap(
        file =>
          if (file.size.isEmpty)
            file.listFiles()
          else
            Seq(file)
      )
    }

    private def listDirectories(): Seq[File] = {
      children.flatMap(
        child =>
          if (child.size.isEmpty)
            child.listDirectories().prepended(child)
          else
            None
      )
    }

    def directorySizes(): Seq[Int] = {
      listDirectories().map(_.filesSize())
    }

    def filesSize(): Int = {
      listFiles().map(_.size.get).sum
    }
  }

  val root = File("/")

  val cd = "$ cd "
  val ls = "$ ls"
  val dir = "dir "

  lines.foldLeft(root)(
    (directory, line) =>
      if (line.startsWith(cd)) {
        val directoryName = line.substring(cd.length)

        if (directoryName == "/")
          root
        else if (directoryName == "..")
          directory.parent
        else
          directory.getChild(directoryName)
      } else if (line == ls) {
        directory
      } else if (line.startsWith(dir)) {
        directory.appendChild(line.substring(dir.length))

        directory
      } else {
        val split = line.split(" ")
        directory.appendChild(split(1), split(0).toIntOption)

        directory
      }
  )

  println("answer1: " + root.directorySizes().filter(_ <= 100000).sum)

  val required = root.filesSize() - 70000000 + 30000000
  println("answer2: " + root.directorySizes().filter(_ >= required).min)
}
