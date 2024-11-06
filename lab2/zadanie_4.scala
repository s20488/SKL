object FibonacciConverter {
  def value(n: Int): Int = {
    @annotation.tailrec
    def valueTailRec(current: Int, prev1: Int, prev2: Int): Int = {
      if (current == n) prev1
      else valueTailRec(current + 1, prev1 + prev2, prev1)
    }

    if (n == 0) 2
    else if (n == 1) 1
    else valueTailRec(2, 3, 2)
  }
}

@main
def mainFibonacciConverter(): Unit = {
  println(FibonacciConverter.value(2))
}
