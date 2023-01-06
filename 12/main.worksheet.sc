import scala.io.Source

type HeightFilter = Function2[Int, Int, Boolean]

case class Position(char: Char, row: Int, col: Int):
  lazy val height = Position.heightMap(char)
  lazy val neighbors =
    List((row + 1, col), (row, col + 1), (row - 1, col), (row, col - 1))

  def getMoves(grid: Seq[Seq[Position]], moveFn: HeightFilter) =
    neighbors.flatMap { (row, col) =>
      grid.lift(row).flatMap(_.lift(col)).filter(p => moveFn(height, p.height))
    }

object Position:
  lazy val heightMap: Map[Char, Int] =
    s"abcdefghijklmnopqrstuvwxyzSE".zipWithIndex.map {
      case ('S', _) => ('S', 1)
      case ('E', _) => ('E', 26)
      case (char, i) => (char, i + 1)
    }.toMap

enum Direction(
  val start: Position => Boolean,
  val end: Position => Boolean,
  val moveFn: HeightFilter
):
  case Regular extends Direction(
    p => p.char == 'S',
    p => p.char == 'E',
    (height, newHeight) => newHeight - height <= 1
  )
  case Reverse extends Direction(
    p => p.char == 'E',
    p => p.height == 1,
    (height, newHeight) => newHeight - height >= -1
  )

case class Move(position: Position, count: Int)

case class MapGrid(grid: Seq[Seq[Position]], direction: Direction):
  def findPosition(f: Position => Boolean) =
    grid.flatMap { row => row.find(pos => f(pos)) }(0)

  lazy val start = findPosition(direction.start)

  def minMoves(moves: List[Move], visited: Set[Position] = Set()): Int =
    val Move(pos, count) = moves.head
    if direction.end(pos) then return count
    visited(pos) match
      case true => minMoves(moves.tail, visited)
      case false =>
        val nextMoves = pos.getMoves(this.grid, direction.moveFn)
          .filterNot(visited(_))
        minMoves(moves.tail ++ nextMoves.map(Move(_, count + 1)), visited + pos)

  def play() = minMoves(List(Move(start, 0)))

object HillClimbing:
  def one() = common(Direction.Regular)
  def two() = common(Direction.Reverse)

  def common(direction: Direction) =
    val lines = Source.fromFile("12/input.txt").getLines.toList
    val map = new MapGrid(
      for (line, i) <- lines.zipWithIndex yield for (char, j) <- line.zipWithIndex
        yield Position(char, i, j),
      direction
    )
    map.play()

HillClimbing.one()
HillClimbing.two()
