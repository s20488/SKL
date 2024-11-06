object StringReverser {
  def reverse(str: String): String = {
    @annotation.tailrec
    def reverseTailRec(remaining: String, acc: String): String = {
      if (remaining.isEmpty) acc
      else reverseTailRec(remaining.tail, remaining.head + acc)
    }
    reverseTailRec(str, "")
  }
}

@main
def mainPrint(): Unit = {
  println(StringReverser.reverse("kot"))
}
