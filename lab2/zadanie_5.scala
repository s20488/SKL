object ArrayOrderChecker {
  def isOrdered(tab: Array[Int], mlr: (Int, Int) => Boolean): Boolean = {
    @annotation.tailrec
    def isOrderedTailRec(index: Int): Boolean = {
      if (index >= tab.length - 1) true
      else if (!mlr(tab(index), tab(index + 1))) false
      else isOrderedTailRec(index + 1)
    }
    isOrderedTailRec(0)
  }
}

@main
def mainArrayOrderChecker(): Unit = {
  val tab1 = Array(1, 3, 3, 6, 8)
  println(ArrayOrderChecker.isOrdered(tab1, _ <= _)) //true
  println(ArrayOrderChecker.isOrdered(tab1, _ < _)) //false
}
