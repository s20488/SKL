object SequenceDifference {

  def diff[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] = {
    seq1.zip(seq2).filter {case (elem1, elem2) => elem1 != elem2}.map {case (elem1, _) => elem1}
  }
}

@main
def mainSequenceDifference(): Unit = {
  val seq1 = Seq(1, 2, 3)
  val seq2 = Seq(2, 2, 1, 3)

  val result = SequenceDifference.diff(seq1, seq2)
  println(s"First sequence: ${seq1.mkString("[", ", ", "]")}")
  println(s"Second sequence: ${seq2.mkString("[", ", ", "]")}")
  println(s"Difference result: ${result.mkString("[", ", ", "]")}")
}
