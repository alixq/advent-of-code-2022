import scala.io.Source

sealed trait Command(val cycles: Int)
object Command:
  def Parse: PartialFunction[String, Command] = {
    case NoOp.Match() => NoOp
    case AddX.Match(count) => AddX(count.toInt)
  }

case object NoOp extends Command(1):
  val Match = "^noop$".r

case class AddX(count: Int) extends Command(2)
object AddX:
  val Match = "^addx\\s([-\\d]+)".r


case class CPU(cycle: Int = 1, x: Int = 1, snapshots: Seq[Int] = Seq()):
  def exec(command: Command) =
    val newX = command match
      case cmd @ AddX(count) => x + count
      case NoOp => x

    copy(cycle + command.cycles, newX, takeSnapshots(command))

  def takeSnapshots(cmd: Command) =
    (0 until cmd.cycles).foldLeft(snapshots) { (snapshots, inc) =>
      snapshots :+ x
    }

case object CPU:
  val snapshotCycles = (20 to 220 by 40).toList

object CathodeRayTube:
  def one() = common { snapshots =>
    val cycles = CPU.snapshotCycles.map { cycle => snapshots(cycle - 1) * cycle }
    cycles.sum
  }

  def two() = common { snapshots =>
    snapshots.grouped(40).foreach { line =>

      val pixels = line.zipWithIndex.map {
        case (z, i) if (z - i).abs <= 1 => "#"
        case _ => "."
      }
      println(pixels.mkString)
    }
  }

  def common(f: Seq[Int] => Any) =
    val lines = Source.fromFile("10/input.txt").getLines.toList
    val commands = for line <- lines yield Command.Parse(line)
    val cpu = commands.foldLeft(CPU()) { (cpu, command) => cpu.exec(command) }
    f(cpu.snapshots)

CathodeRayTube.one()
CathodeRayTube.two()
