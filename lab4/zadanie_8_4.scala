object MedianCalculator {

  def median(seq: Seq[(String, Double)]): Double = {
    val sortedScores = for {
      (_, score) <- seq
    } yield score
    val sorted = sortedScores.sorted

    val size = sorted.size
    if (size % 2 == 1)
      sorted(size / 2)
    else {
      val mid = size / 2
      (sorted(mid - 1) + sorted(mid)) / 2
    }
  }
}

@main
def mainMedianCalculator(): Unit = {
  val seq = Seq(
    ("Alice", 85.0),
    ("Bob", 90.0),
    ("Charlie", 78.0),
    ("Diana", 88.0),
    ("Eve", 92.0)
  )

  val result = MedianCalculator.median(seq)

  println(s"Scores: ${seq.map(_._2).mkString("[", ", ", "]")}")
  println(s"Median: $result")
}
