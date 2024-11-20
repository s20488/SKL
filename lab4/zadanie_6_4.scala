object IsOrdered {

  def isOrdered[A](seq: Seq[A])(leq: (A, A) => Boolean): Boolean = {
    seq.sliding(2).forall {
      case Seq(a, b) => leq(a, b)
      case _ => true
    }
  }
}

@main
def mainIsOrdered(): Unit = {
  val seq1 = Seq(1, 2, 2, 4)
  val seq2 = Seq(1, 3, 2, 4)

  val leq1: (Int, Int) => Boolean = _ < _
  val leq2: (Int, Int) => Boolean = _ <= _

  println(s"Sequence: ${seq1.mkString("[", ", ", "]")}, Predicate: (_ < _), Is ordered: ${IsOrdered.isOrdered(seq1)(leq1)}")
  println(s"Sequence: ${seq1.mkString("[", ", ", "]")}, Predicate: (_ <= _), Is ordered: ${IsOrdered.isOrdered(seq1)(leq2)}")
  println(s"Sequence: ${seq2.mkString("[", ", ", "]")}, Predicate: (_ <= _), Is ordered: ${IsOrdered.isOrdered(seq2)(leq2)}")
}
