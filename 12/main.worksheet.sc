import scala.io.Source

case class Position(height: Int, char: Char, row: Int, col: Int):
  lazy val isStart = char == 'S'
  lazy val isEnd = char == 'E'

  lazy val neighbors =
    List((row + 1, col), (row, col + 1), (row - 1, col), (row, col - 1))

  def getMoves(grid: Seq[Seq[Position]], hist: Seq[Position]) =
    neighbors.collect {
      case (row, col) => grid.lift(row).flatMap(_.lift(col))
    }.flatten.filter {
      case Position(newHeight, _, _, _) if newHeight - height >= 2 => false
      case pos @ _:Position if hist.exists(p => p == pos) => false
      case _ => true
    }

object Position:
  lazy val heightMap: Map[Char, Int] =
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    s"${alphabet}SE".zipWithIndex.map {
      case ('S', _) => ('S', 1)
      case ('E', _) => ('E', 26)
      case (char, i) => (char, i + 1)
    }.toMap

  def apply(char: Char, row: Int, col: Int) =
    new Position(heightMap(char), char, row, col)

case class Move(pos: Position, nextMoves: Seq[Move]):
  def minMovesLeft(current: Int = 1): Int =
    if pos.isEnd then return current
    if nextMoves.isEmpty then return -1
    val posNextMoves = nextMoves.map(_.minMovesLeft(current + 1)).filter(_ > 0)
    if posNextMoves.isEmpty then return -1 else posNextMoves.min

case class MapGrid(grid: Vector[Vector[Position]]):
  def findPosition(f: Position => Boolean) =
    val coords = for {
      (row, i) <- grid.zipWithIndex
      (pos, j) <- row.zipWithIndex if f(pos)
    } yield pos
    coords(0)

  lazy val start = findPosition(_.isStart)
  lazy val end = findPosition(_.isEnd)

  def row(index: Int) = grid(index)
  def col(index: Int) = grid.map(_(index))

  def playMove(pos: Position, history: Seq[Position] = Seq()): Seq[Move] =
    val nextMoves = pos.getMoves(this.grid, history)
    if nextMoves.isEmpty || pos == end then return Seq()
    nextMoves.map { pos => Move(pos, playMove(pos, history :+ pos)) }

  def play() =
    val moveTree = playMove(start)
    moveTree.map(m => m.minMovesLeft())

object MapGrid:
  def apply(lines: Seq[String]) = new MapGrid(
    lines.zipWithIndex.map {
      (line, i) => line.zipWithIndex.map {
        (char, j) => Position(char, i, j)
      }.toVector
    }.toVector
  )

object HillClimbing:
  def one() = common()
  def two() = common()

  def common() =
    val lines = Source.fromFile("12/example.txt").getLines.toList
    val map = MapGrid(lines)
    // map.play()


HillClimbing.one()
HillClimbing.two()
