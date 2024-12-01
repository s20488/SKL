object SequenceSwapper {

  def swap[A](seq: Seq[A]): Seq[A] = {
    seq.grouped(2).flatMap {
      case Seq(a, b) => Seq(b, a)
      case Seq(a) => Seq(a)
    }.toSeq
  }
}

@main
def mainSequenceSwapper(): Unit = {
  val seq = Seq(1, 2, 3, 4, 5)
  val swappedSeq = SequenceSwapper.swap(seq)
  println(s"Original sequence: ${seq.mkString("[", ", ", "]")}")
  println(s"Swapped sequence: ${swappedSeq.mkString("[", ", ", "]")}")
}
