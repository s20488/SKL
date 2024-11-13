object Compressor {
  def compress[A](list: List[A]): List[(A, Int)] = {
    @annotation.tailrec
    def loop(remaining: List[A], currentElement: A, count: Int, result: List[(A, Int)]): List[(A, Int)] = remaining match {
      case Nil => (currentElement, count) :: result
      case x :: xs if x == currentElement => loop(xs, currentElement, count + 1, result)
      case x :: xs => loop(xs, x, 1, (currentElement, count) :: result)
    }

    list match {
      case Nil => List.empty
      case x :: xs => loop(xs, x, 1, List.empty).reverse
    }
  }
}

@main
def mainCompressor(): Unit = {
  val list = List('a', 'a', 'b', 'c', 'c', 'c', 'd', 'd', 'c')
  val result = Compressor.compress(list)

  println(s"Input list: ${list.mkString("[", ", ", "]")}")
  println(s"Compressed result: ${result.mkString("[", ", ", "]")}")
}
