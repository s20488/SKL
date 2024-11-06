object BinaryConverter {
  def binToDec(bin: Int): Int = {
    @annotation.tailrec
    def binToDecTailRec(remainingBin: Int, powerOfTwo: Int, acc: Int): Int = {
      if (remainingBin == 0) acc
      else {
        val lastDigit = remainingBin % 10
        val newAcc = acc + lastDigit * powerOfTwo
        binToDecTailRec(remainingBin / 10, powerOfTwo * 2, newAcc)
      }
    }
    binToDecTailRec(bin, 1, 0)
  }
}

@main
def mainBinaryConverter(): Unit = {
  println(BinaryConverter.binToDec(1000))
}
