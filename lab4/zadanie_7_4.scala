object FrequencyCounter {

  def freq[A](seq: Seq[A]): Set[(A, Int)] = {
    seq.groupBy(identity).map { case (element, occurrences) =>
      (element, occurrences.size)
    }.toSet
  }
}

@main
def mainFrequencyCounter(): Unit = {
  val seq = Seq('a', 'b', 'a', 'c', 'c', 'a')

  val result = FrequencyCounter.freq(seq)

  println(s"Sequence: ${seq.mkString("[", ", ", "]")}")
  println(s"Frequencies: ${result.mkString("{", ", ", "}")}")
}
