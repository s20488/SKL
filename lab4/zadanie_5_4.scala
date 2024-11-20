object DeStutter {

  def deStutter[A](seq: Seq[A]): Seq[A] = {
    seq.foldLeft(Seq.empty[A]) {
      case (result, elem) if result.isEmpty || result.last != elem => result :+ elem
      case (result, _) => result
    }
  }
}

@main
def mainDeStutter(): Unit = {
  val seq = Seq(1, 1, 2, 4, 4, 4, 1, 3)
  val result = DeStutter.deStutter(seq)

  println(s"Input sequence: ${seq.mkString("[", ", ", "]")}")
  println(s"Sequence after removing duplicates: ${result.mkString("[", ", ", "]")}")
}
