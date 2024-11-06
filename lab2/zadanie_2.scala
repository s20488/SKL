object PrimeCheck {
  def isPrime(n: Int): Boolean = {
    @annotation.tailrec
    def isPrimeTailRec(divisor: Int): Boolean = {
      if (divisor * divisor > n) true
      else if (n % divisor == 0) false
      else isPrimeTailRec(divisor + 1)
    }

    if (n <= 1) false
    else isPrimeTailRec(2)
  }
}

@main
def mainPrimeCheck(): Unit = {
  println(PrimeCheck.isPrime(5))
}
