object SubListChecker {
  def isSub[A](l: List[A], lSub: List[A]): Boolean = {
    @annotation.tailrec
    def loop(remainingSub: List[A], remainingL: List[A]): Boolean = (remainingSub, remainingL) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (x :: xs, y :: ys) if x == y => loop(xs, ys)
      case (x :: xs, y :: ys) => loop(x :: xs, ys)
    }

    loop(lSub, l)
  }
}

@main
def mainSubListChecker(): Unit = {
  val l = List('b', 'o', 'c', 'i', 'a', 'n')
  val lSub = List('a', 'b', 'c')
  val result = SubListChecker.isSub(l, lSub)
  println(result) // Result: True
}