object ArrayWorthCalculator {
  def worth(tab1: Array[Int], tab2: Array[Int])(pred: (Int, Int) => Boolean)(op: (Int, Int) => Int): Option[Int] = {
    @annotation.tailrec
    def worthTailRec(index: Int): Option[Int] = {
      if (index >= tab1.length || index >= tab2.length) None
      else if (pred(tab1(index), tab2(index))) Some(op(tab1(index), tab2(index)))
      else worthTailRec(index + 1)
    }
    worthTailRec(0)
  }
}

@main
def mainArrayWorthCalculator(): Unit = {
  val tab1 = Array(-1, 3, 2, -8, 5)
  val tab2 = Array(-3, 3, 3, 0, -4, 5)
  println(ArrayWorthCalculator.worth(tab1, tab2)(_ < _)(_ + _)) // Some(5)
}
