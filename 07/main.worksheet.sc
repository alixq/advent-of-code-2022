import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map

// NODES

sealed trait Node:
  def parent: Option[Folder]
  def size: Int

case class File(size: Int, parent: Option[Folder]) extends Node
case object File:
  def Match = "^(\\d+)\\s(.+)$".r

case class Folder(parent: Option[Folder]) extends Node:
  val children = Map[String, Node]()
  def size = children.values.map(_.size).sum
  def root: Folder = if parent == None then this else parent.get.root

case object Folder:
  def Match = "^dir\\s(.+)$".r


// COMMANDS

sealed trait Command:
  def path: Folder
  def run(): Folder

case class CD(path: Folder, to: String) extends Command:
  def run() = to match
    case ".." => path.parent.get
    case "/" => path.root
    case _ =>
      (for case f @ Folder(_) <- path.children.get(to) yield f).get


case object CD:
  val Match = "^\\$ cd (.+)$".r


case class LS(path: Folder, output: Seq[String]) extends Command:
  def run() =
    output foreach { line =>
      line match {
        case File.Match(size, name) =>
          path.children.addOne (name, File(size.toInt, Some(path)))
        case Folder.Match(name) =>
          path.children.addOne (name, Folder(Some(path)))
      }
    }
    path

case object LS:
  val Match = "^\\$ ls".r

// EXEC

object NoSpaceLeftOnDevice:
  def one() = common { sizes =>
    sizes.filter(_ <= 100_000).sum
  }

  def two() = common { sizes =>
    val totalUsed = sizes.max
    println(totalUsed)
    val needToFree = totalUsed - (70_000_000 - 30_000_000)
    sizes.sorted.find(_ >= needToFree)
  }

  def common(f: (Seq[Int] => Any)) =
    val lines = Source.fromFile("07/input.txt").getLines.toList
    val folder = parseCommands(lines)

    def retrieveFolderSizes(nodes: List[Node]): List[Int] =
      val folders = for case f @ Folder(_) <- nodes yield f
      if folders.isEmpty then return List()
      val subNodes = folders.flatMap(_.children.values)
      folders.map(_.size) concat retrieveFolderSizes(subNodes)

    val folderSizes = folder.size :: retrieveFolderSizes(List(folder))
    f(folderSizes)

  def parseCommands(lines: Seq[String]) =
    def parseNext(lines: Seq[String], folder: Folder): Folder =
      if lines.isEmpty then return folder.root
      val cmdLines = lines.tail.takeWhile(line => !line.startsWith("$"))
      val newFolder: Folder = lines.head match {
        case CD.Match(path) => CD(folder, path).run()
        case LS.Match() => LS(folder, cmdLines).run()
      }
      parseNext(lines.drop(cmdLines.size + 1), newFolder)

    parseNext(lines, Folder(None))

NoSpaceLeftOnDevice.one()
NoSpaceLeftOnDevice.two()
