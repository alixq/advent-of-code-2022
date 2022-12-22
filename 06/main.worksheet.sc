import scala.io.Source

object TuningTrouble:
  def one() = common(4)
  def two() = common(14)

  def common(n: Int) =
    val lines = Source.fromFile("06/input.txt").getLines.toList
    val results = lines.map { line =>
      line.zipWithIndex.indexWhere { (_, i) =>
        val slice = line.slice(i - (n - 1), i + 1)
        slice.size == n && slice.distinct.size == slice.size
      } + 1
    }
    println(results)

TuningTrouble.one()
TuningTrouble.two()
