import scala.io.StdIn._

object PrimeSumChecker {

  def isPrime(num: Int): Boolean = {
    if (num < 2) false
    else (2 to Math.sqrt(num).toInt).forall(i => num % i != 0)
  }

  def generatePrimes(n: Int): List[Int] = {
    (2 to n).filter(isPrime).toList
  }

  def main(args: Array[String]): Unit = {
    println("Podaj naturalna liczbe parzysta > 2:")
    val n = readInt()

    if (n > 2) {
      val primes = generatePrimes(n)

      for (even <- 4 to n by 2) {
        var found = false

        for (i <- primes) {
          val complement = even - i
          if (complement >= 2 && primes.contains(complement)) {
            println(s"$i + $complement = $even")
            found = true
          }
        }
      }
    } else {
      println("Liczba nie odpowiada warunkom zadania")
    }
  }
}
