import scala.io.Source
import scala.util.matching.Regex

case class Item(worry: Int, relief: Int => Int):
  def inspect(op: Int => Int) =
    println((worry, relief(op(worry))))
    copy(worry=relief(op(worry)))

case class WorryTest(divider: Int, outcomes: (Int, Int)):
  def send(item: Item) =
    if item.worry % divider == 0 then outcomes._1 else outcomes._2

case class MonkeyBag(items: Seq[Item], inspections: Int = 0):
  def addItem(item: Item) = copy(items :+ item)

  def inspectItem() =
    val newBag = copy(items.tail, inspections + 1)
    (items.head, newBag)

case class Throw(recipient: Int, item: Item)

case class Monkey(op: Int => Int, test: WorryTest, bag: MonkeyBag):
  def playTurn(monkeys: Map[Int, Monkey], myIndex: Int): Map[Int, Monkey] =
    if bag.items.size == 0 then return monkeys
    val (item, newBag) = bag.inspectItem()
    val newSelf = copy(bag = newBag)
    val newItem = item.inspect(op)
    val newMonkeys = monkeys.updated(myIndex, newSelf)
      .updatedWith(test.send(newItem)) {
        case Some(monkey) => Some(monkey.receive(newItem))
        case None => None
      }
    newSelf.playTurn(newMonkeys, myIndex)


  def receive(item: Item) = copy(bag = bag.addItem(item))

object Monkey:
  def parse(lines: String, relief: Int => Int) =
    val ind = matchGroup("Monkey (\\d+)".r, lines)
    val worries = matchGroup("Starting items: ([\\d\\s,]+)\n".r, lines)
    val op = parseOperation(lines)
    val div = matchGroup("Test: divisible by (\\d+)".r, lines)
    val outcomes = "If (true|false): throw to monkey (\\d+)".r
      .findAllMatchIn(lines).map(_.group(2).toInt).toList

    val items = worries.split(", ").map(w => Item(w.toInt, relief)).toList
    val worryTest = WorryTest(div.toInt, (outcomes(0), outcomes(1)))
    (ind.toInt, Monkey(op, worryTest, MonkeyBag(items)))

  def matchGroup(regex: Regex, str: String, ind: Int = 1) =
    str match { case regex.unanchored(group) => group }

  def parseOperation(lines: String) =
    val reg = raw"Operation: new = old (\+|\*) (old|\d+)\n".r.unanchored
    lines match
      case reg("*", "old") => (old: Int) => old * old
      case reg("+", "old") => (old: Int) => old + old
      case reg("*", n) => (old: Int) => old * n.toInt
      case reg("+", n) => (old: Int) => old + n.toInt

type MonkeyMap = Map[Int, Monkey]

object MonkeyInTheMiddle:
  def one() = common(20, w => (w.toDouble / 3).floor.toInt)
  def two() = common(10_000)

  def common(rounds: Int, relief: Int => Int = identity) =
    val lines = Source.fromFile("11/input.txt").getLines.toList
    val monkeys = playRounds(parseMonkeys(lines, relief), rounds)
    monkeys.toList.map { (_i, m) => BigInt(m.bag.inspections) }
      .sorted.reverse.slice(0, 2).product

  def parseMonkeys(
    lines: Seq[String],
    relief: Int => Int
  ): MonkeyMap =
    def parseMonkey(linesLeft: Seq[String], monkeys: MonkeyMap): MonkeyMap =
      if linesLeft.isEmpty then return monkeys
      val monkeyLines = linesLeft.takeWhile(_ != "")
      val monkeyTup = Monkey.parse(monkeyLines.mkString("\n"), relief)
      val linesLeftToParse = linesLeft.drop(monkeyLines.size + 1)
      parseMonkey(linesLeftToParse, monkeys + monkeyTup)

    parseMonkey(lines, Map())

  def playRounds(monkeys: Map[Int, Monkey], rounds: Int) =
    val orderedMonkeys = monkeys.toList.sortBy(_._1)
    val turns =
      for (round <- (1 to rounds) ; (i, _monkey) <- orderedMonkeys)
      yield (round, i)

    turns.foldLeft(monkeys) { case (monkeys, (round, monkeyInd)) =>
      if List(1, 20, 1000, 2000, 3000, 1000) contains (round - 1) then
        println((round, monkeys.toList.map { (_i, m) => m.bag.inspections }))
      monkeys(monkeyInd).playTurn(monkeys, monkeyInd)
    }


MonkeyInTheMiddle.one()
MonkeyInTheMiddle.two()
