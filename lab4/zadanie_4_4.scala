object OptionSum {

  def sumOption(seq: Seq[Option[Double]]): Double = {
    seq.foldLeft(0.0) {
      case (sum, Some(value)) => sum + value
      case (sum, None) => sum
    }
  }
}

@main
def mainOptionSum(): Unit = {
  val seq = Seq(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
  val result = OptionSum.sumOption(seq)

  println(s"Input sequence: ${seq.mkString("[", ", ", "]")}")
  println(s"Sum of elements: $result")
}
