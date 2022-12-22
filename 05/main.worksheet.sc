import scala.io.Source
import scala.collection.mutable.Stack

case class Move(quantity: Int, from: Int, to: Int)

object SupplyStacks:
  def one() = common(executeMovesOne)
  def two() = common(executeMovesTwo)

  def common(execMoves: Function2[Seq[Stack[Char]], Seq[Move], Unit]) =
    val lines = Source.fromFile("05/input.txt").getLines.toList
    val initialStateLines = lines.takeWhile(_ != "")
    val movesLines = lines.drop(initialStateLines.size + 1)

    val stacksColIndexes = retrieveColIndexes(initialStateLines.last)
    val stacks = fillStacks(stacksColIndexes, initialStateLines.init)

    val moves = parseMoves(movesLines)
    execMoves(stacks, moves)
    println(stacks.map(_.top))

  def retrieveColIndexes(numbers: String) =
    for (char, i) <- numbers.zipWithIndex if char.isDigit yield i

  def fillStacks(stacksColIndexes: Seq[Int], initialState: Seq[String]) =
    stacksColIndexes map { colIndex =>
      new Stack[Char].addAll {
        for line <- initialState
            char <- line.lift(colIndex) if !char.isSpaceChar
        yield line(colIndex)
      }
    }

  def parseMoves(lines: Seq[String]) =
    lines collect { line =>
      "\\d+".r.findAllIn(line).toList match
        case quantity :: from :: to :: _ =>
          Move(quantity.toInt, from.toInt - 1, to.toInt - 1)
    }

  def executeMovesOne(stacks: Seq[Stack[Char]], moves: Seq[Move]) =
    moves foreach { case Move(quantity, from, to) =>
      (1 to quantity) foreach (_ => stacks(to).push(stacks(from).pop()))
    }

  def executeMovesTwo(stacks: Seq[Stack[Char]], moves: Seq[Move]) =
    moves foreach { case Move(quantity, from, to) =>
      val items = (1 to quantity).map { _ => stacks(from).pop() }.reverse
      items.foreach { item => stacks(to).push(item) }
    }

SupplyStacks.one()
SupplyStacks.two()
