object MinMaxCalculator {

  def minMax(seq: Seq[(String, Double)]): Option[(String, String)] = {
    if (seq.isEmpty) {
      None
    } else {
      val minUser = seq.minBy(_._2)
      val maxUser = seq.maxBy(_._2)

      Some((minUser._1, maxUser._1))
    }
  }
}

@main
def mainMinMaxCalculator(): Unit = {
  val seq = Seq(
    ("Alice", 85.0),
    ("Bob", 90.0),
    ("Charlie", 78.0),
    ("Diana", 88.0),
    ("Eve", 92.0)
  )

  val result = MinMaxCalculator.minMax(seq)

  result match {
    case Some((minUser, maxUser)) =>
      println(s"User with minimum points: $minUser")
      println(s"User with maximum points: $maxUser")
    case None => println("The sequence is empty.")
  }
}
