object SubSequence {

  def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] = {
    for {
      (element, index) <- seq.zipWithIndex
      if index >= begIdx && index < endIdx
    } yield element
  }
}

@main
def mainSubSequence(): Unit = {
  val seq = Seq(1, 2, 3, 4, 5, 6, 7)
  val begIdx = 2
  val endIdx = 5

  val result = SubSequence.subSeq(seq, begIdx, endIdx)
  println(s"Original sequence: ${seq.mkString("[", ", ", "]")}")
  println(s"Subsequence from index $begIdx to $endIdx: ${result.mkString("[", ", ", "]")}")
}


//object SubSequence {
//
//  def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] = {
//    val p = seq.take(begIdx)
//    p.drop(endIdx)
//  }
//}
//
//@main
//def mainSubSequence(): Unit = {
//  val p = for {
//    1 <- (1 to 10) if (1 % 2 == 0)
//    2 <- {1 to 10}
//  } yield  (1, 3)
//  println(p.toList)
//}