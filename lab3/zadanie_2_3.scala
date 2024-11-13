object Merger {
  def merge[A](a: List[A], b: List[A])(leq: (A, A) => Boolean): List[A] = {
    @annotation.tailrec
    def loop(aRemaining: List[A], bRemaining: List[A], result: List[A]): List[A] = (aRemaining, bRemaining) match {
      case (Nil, Nil) => result
      case (Nil, x :: xs) => loop(Nil, xs, x :: result)
      case (x :: xs, Nil) => loop(xs, Nil, x :: result)
      case (x :: xs, y :: ys) if leq(x, y) => loop(xs, bRemaining, x :: result)
      case (x :: xs, y :: ys) => loop(aRemaining, ys, y :: result)
    }

    loop(a, b, List.empty).reverse
  }
}

@main
def mainMerger(): Unit = {
  val a = List(1, 3, 5, 8)
  val b = List(2, 4, 6, 8, 10, 12)
  val leq: (Int, Int) => Boolean = (m, n) => m < n
  val result = Merger.merge(a, b)(leq)

  println(s"Merged result: ${result.mkString("[", ", ", "]")}")
}
