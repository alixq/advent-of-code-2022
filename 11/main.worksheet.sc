import scala.io.Source
import scala.util.matching.Regex

case class Item(worry: BigInt, relief: BigInt => BigInt, modulus: Int):
  def inspect(op: BigInt => BigInt) =
    copy(worry = relief(op(worry)) % modulus)

case class WorryTest(divider: Int, ifDivisible: Int, ifNot: Int):
  def send(item: Item) =
    if item.worry % divider == 0 then ifDivisible else ifNot

case class MonkeyBag(items: Seq[Item], inspections: Int = 0):
  def addItem(item: Item) = copy(items :+ item)

  def inspectItem() =
    val newBag = copy(items.tail, inspections + 1)
    (items.head, newBag)

case class Monkey(op: BigInt => BigInt, test: WorryTest, bag: MonkeyBag):
  def playTurn(monkeys: MonkeyMap, myIndex: Int): MonkeyMap =
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

case class ParsedMonkeyData(
  index: Int,
  itemWorries: Seq[Int],
  op: BigInt => BigInt,
  div: Int,
  ifDivisible: Int,
  ifNot: Int
)

object Monkey:
  def parse(lines: String) =
    val worries = matchGroup("Starting items: ([\\d\\s,]+)\n".r, lines)
    val outcomes = "If (true|false): throw to monkey (\\d+)".r
      .findAllMatchIn(lines).map(_.group(2).toInt).toList

    ParsedMonkeyData(
      itemWorries = worries.split(", ").map(_.toInt),
      index = matchGroup("Monkey (\\d+)".r, lines).toInt,
      op = parseOperation(lines),
      div = matchGroup("Test: divisible by (\\d+)".r, lines).toInt,
      ifDivisible = outcomes(0).toInt,
      ifNot = outcomes(1).toInt
    )

  def makeMap(parsed: Seq[ParsedMonkeyData], relief: BigInt => BigInt) =
    val modulus = parsed.map(_.div).product
    parsed.foldLeft[MonkeyMap](Map()) { (monkeys, parsed) =>
      val items = parsed.itemWorries.map(w => Item(w, relief, modulus)).toList
      val worryTest = WorryTest(parsed.div.toInt, parsed.ifDivisible, parsed.ifNot)
      val monkeyTup = (parsed.index, Monkey(parsed.op, worryTest, MonkeyBag(items)))
      monkeys + monkeyTup
    }

  def matchGroup(regex: Regex, str: String, ind: Int = 1) =
    str match { case regex.unanchored(group) => group }

  def parseOperation(lines: String) =
    val reg = raw"Operation: new = old (\+|\*) (old|\d+)\n".r.unanchored
    lines match
      case reg("*", "old") => (old: BigInt) => old * old
      case reg("+", "old") => (old: BigInt) => old + old
      case reg("*", n) => (old: BigInt) => old * n.toInt
      case reg("+", n) => (old: BigInt) => old + n.toInt

type MonkeyMap = Map[Int, Monkey]

object MonkeyInTheMiddle:
  def one() = common(20, w => (w.toDouble / 3).floor.toInt)
  def two() = common(10_000)

  def common(rounds: Int, relief: BigInt => BigInt = identity) =
    val lines = Source.fromFile("11/input.txt").getLines.toList
    val monkeys = playRounds(parseMonkeys(lines, relief), rounds)
    monkeys.toList.map { (_i, m) => BigInt(m.bag.inspections) }
      .sorted.reverse.slice(0, 2).product

  def parseMonkeys(lines: Seq[String], relief: BigInt => BigInt): MonkeyMap =
    def parseMonkey(
      linesLeft: Seq[String],
      monkeys: Seq[ParsedMonkeyData]
    ): Seq[ParsedMonkeyData] =
      if linesLeft.isEmpty then return monkeys
      val monkeyLines = linesLeft.takeWhile(_ != "")
      val linesLeftToParse = linesLeft.drop(monkeyLines.size + 1)
      val newMonkeys = monkeys :+ Monkey.parse(monkeyLines.mkString("\n"))
      parseMonkey(linesLeftToParse, newMonkeys)

    val parsedMonkeys = parseMonkey(lines, List())
    Monkey.makeMap(parsedMonkeys, relief)

  def playRounds(monkeys: MonkeyMap, rounds: Int) =
    val turns = for (round <- (1 to rounds); i <- monkeys.keys.toList.sorted)
                yield (round, i)

    turns.foldLeft(monkeys) { case (monkeys, (round, monkeyInd)) =>
      monkeys(monkeyInd).playTurn(monkeys, monkeyInd)
    }


MonkeyInTheMiddle.one()
MonkeyInTheMiddle.two()
