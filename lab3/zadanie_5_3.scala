object Computation {
  def compute[A, B](l: List[Option[A]])(op1: A => B)(op2: (A, B) => B): Option[B] = {
    @annotation.tailrec
    def loop(remaining: List[Option[A]], current: Option[B]): Option[B] = remaining match {
      case Nil => current
      case None :: xs => loop(xs, current)
      case Some(x) :: xs => current match {
        case None => loop(xs, Some(op1(x)))
        case Some(c) => loop(xs, Some(op2(x, c)))
      }
    }

    loop(l, None)
  }
}

@main
def mainComputation(): Unit = {
  val l = List(Some(1), None, Some(2), None, Some(3), Some(4))
  val op1: Int => Int = _ + 0
  val op2: (Int, Int) => Int = _ + _
  val result = Computation.compute(l)(op1)(op2)

  println(s"Computed result: ${result.mkString("[", ", ", "]")}")
}
