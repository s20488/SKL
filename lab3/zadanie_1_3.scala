object Divider {
  def divide[A](list: List[A]): (List[A], List[A]) = {
    @annotation.tailrec
    def loop(remaining: List[A], evens: List[A], odds: List[A], isEven: Boolean): (List[A], List[A]) = remaining match {
      case Nil => (evens.reverse, odds.reverse)
      case x :: xs =>
        if (isEven) loop(xs, x :: evens, odds, !isEven)
        else loop(xs, evens, x :: odds, !isEven)
    }

    loop(list, Nil, Nil, isEven = true)
  }
}

@main
def mainDivider(): Unit = {
  val list = List(1, 3, 5, 6, 7)
  val result = Divider.divide(list)

  println(s"Input list: $list")
  println(s"Result: $result")
}
