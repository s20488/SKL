object CharCounter {

  def countChars(str: String): Int = {
    str.toSet.size
  }
}

@main
def mainCharCounter(): Unit = {
  val exampleString = "Hello, World!"
  val uniqueCharCount = CharCounter.countChars(exampleString)
  println(s"Number of different characters in the inscription: $uniqueCharCount")
}
