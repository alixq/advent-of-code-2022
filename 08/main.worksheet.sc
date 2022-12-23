import scala.io.Source

case class Tree(height: Int, row: Int, col: Int, grid: Grid):
  def allShorter(trees: Seq[Tree]) = trees.forall(_.height < height)

  lazy val isEdge =
    row == 0 || col == 0 || row == grid.rows - 1 || col == grid.cols - 1

  lazy val topTrees = grid.col(col).slice(0, row).reverse
  lazy val bottomTrees = grid.col(col).slice(row + 1, grid.rows)
  lazy val leftTrees = grid.row(row).slice(0, col).reverse
  lazy val rightTrees = grid.row(row).slice(col + 1, grid.cols)
  lazy val allDirections = List(topTrees, bottomTrees, leftTrees, rightTrees)

  lazy val isVisible = isEdge || allDirections.exists(allShorter)

  lazy val scenicScore =
    allDirections.map { trees =>
      val closestTallTree = trees.indexWhere(_.height >= height)
      if closestTallTree == -1 then trees.size else closestTallTree + 1
    }.product

case class Grid(val rows: Int, val cols: Int):
  var trees: Seq[Seq[Tree]] | Null = null

  def fill(lines: Seq[String]) =
    trees = for (line, i) <- lines.zipWithIndex yield {
      for (char, j) <- line.zipWithIndex
      yield Tree(char.asDigit, i, j, this)
    }

  def row(index: Int) = trees(index)
  def col(index: Int) = trees.map(_(index))

  def visibleTrees = trees.map { r => r.map(_.isVisible) }
  def visibleCount = visibleTrees.flatten.count(identity)

  def scenicScores = trees.map { r => r.map(_.scenicScore) }
  def maxScenicScore = scenicScores.flatten.max

object Grid:
  def apply(lines: Seq[String]): Grid =
    val grid = new Grid(rows=lines.length, cols=lines.head.length)
    grid.fill(lines)
    return grid

object TreetopTreeHouse:
  def one() = common { grid => grid.visibleCount }
  def two() = common { grid => grid.maxScenicScore }

  def common(f: Grid => Any) =
    val lines = Source.fromFile("08/input.txt").getLines.toList
    f(Grid(lines))


TreetopTreeHouse.one()
TreetopTreeHouse.two()
