object PythagoreanTriples {

  def threeNumbers(n: Int): Set[(Int, Int, Int)] = {
    (for {
      a <- 1 to n
      b <- a + 1 to n
      c <- b + 1 to n
      if a * a + b * b == c * c
    } yield (a, b, c)).toSet
  }
}

@main
def mainPythagoreanTriples(): Unit = {
  val n = 20
  val result = PythagoreanTriples.threeNumbers(n)

  println(s"Pythagorean triples up to $n: ${result.mkString("{", ", ", "}")}")
}
