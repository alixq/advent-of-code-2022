import scala.io.Source

sealed trait Knot:
  def x: Int
  def y: Int

  def nextPosition(prev: Knot) =
    println((prev, (x, y), (prev.x - x, prev.y - y)))
    (prev.x - x, prev.y - y) match
      case (a, b) if a.abs <= 1 && b.abs <= 1 => (x, y)
      case (0, b) if b.abs == 2 => (x, y + b.sign)
      case (a, 0) if a.abs == 2 => (x + a.sign, y)
      case (a, b) => (x + a.sign, y + b.sign)

case class Head(x: Int, y: Int) extends Knot:
  def move(move: Move) = move match {
    case Move.Left => copy(x = x-1)
    case Move.Right => copy(x = x+1)
    case Move.Up => copy(y = y+1)
    case Move.Down => copy(y = y-1)
  }

case class Middle(x: Int, y: Int) extends Knot:
  def move(prev: Knot) =
    val (x, y) = nextPosition(prev)
    copy(x, y)

case class Tail(x: Int, y: Int, history: Set[(Int, Int)] = Set((0, 0))) extends Knot:
  def move(prev: Knot) =
    val pos = nextPosition(prev)
    copy(pos._1, pos._2, history + pos)

case class Rope(knots: Seq[Knot]):
  def move(move: Move) =
    copy {
      knots.foldLeft(Vector[Knot]()) {
        case (knots, h:Head) => knots :+ h.move(move)
        case (knots, m:Middle) => knots :+ m.move(knots.last)
        case (knots, t:Tail) => knots :+ t.move(knots.last)
      }
    }

  def moveManyTimes(moves: Seq[Move]) =
    moves.foldLeft(this) { (rope, move) => rope.move(move) }

  def tailMoves = Rope.tailHistory(this.knots.last)


object Rope:
  def apply(length: Int) =
    val knots = (1 to length) map {
      case 1 => Head(0, 0)
      case `length` => Tail(0, 0)
      case _ => Middle(0, 0)
    }
    new Rope(knots)

  def tailHistory: PartialFunction[Knot, Int] = {
    case t:Tail => t.history.size
  }

enum Move:
  case Left, Right, Up, Down

object Move:
  val Match = "^(L|R|U|D)\\s(\\d+)$".r

  def parseDir: PartialFunction[String, Move] = {
    case "L" => Left
    case "R" => Right
    case "U" => Up
    case "D" => Down
  }


object RopeBridge:
  def one() = common(2)
  def two() = common(10)

  def common(length: Int) =
    val lines = Source.fromFile("09/input.txt").getLines.toList
    val moves = parseMoves(lines)
    execMoves(moves, length)

  def parseMoves(lines: Seq[String]) = lines.flatMap { line =>
    line match {
      case Move.Match(dir, n) => (1 to n.toInt).map(_ => Move.parseDir(dir))
    }
  }

  def execMoves(moves: Seq[Move], length: Int) =
    val rope = Rope(length).moveManyTimes(moves)
    rope.tailMoves

RopeBridge.one()
RopeBridge.two()
