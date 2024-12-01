object MasterMind {

  def score(code: Seq[Int])(move: Seq[Int]): (Int, Int) = {
    val blacks = (for {
      (c, m) <- code.zip(move) if c == m
    } yield c).size

    val unmatchedCode = (for {
      (c, m) <- code.zip(move) if c != m
    } yield c)
    val unmatchedMove = (for {
      (c, m) <- code.zip(move) if c != m
    } yield m)

    val whiteCount = unmatchedMove.groupBy(identity).map {
      case (color, moves) =>
        math.min(moves.size, unmatchedCode.count(_ == color))
    }.sum

    (blacks, whiteCount)
  }
}

@main
def mainMasterMind(): Unit = {
  val code = Seq(1, 3, 2, 2, 4, 5)
  val move = Seq(2, 1, 2, 4, 7, 2)
  val result = MasterMind.score(code)(move)
  println(s"Code: ${code.mkString("[", ", ", "]")}")
  println(s"Move: ${move.mkString("[", ", ", "]")}")
  println(result)
}
