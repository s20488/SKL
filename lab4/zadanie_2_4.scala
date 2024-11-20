object RemoveElements {

  def remElems[A](seq: Seq[A], k: Int): Seq[A] = {
    seq.zipWithIndex.filter {case (_, index) => index != k}.map {case (element, _) => element}
  }
}

@main
def mainRemoveElements(): Unit = {
  val seq = Seq(1, 2, 3, 4, 5, 6, 7)
  val k = 3

  val result = RemoveElements.remElems(seq, k)
  println(s"Original sequence: ${seq.mkString("[", ", ", "]")}")
  println(s"Sequence after removing $k-th element: ${result.mkString("[", ", ", "]")}")
}
