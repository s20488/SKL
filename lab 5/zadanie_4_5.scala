@main
def zad4: Unit = {
  val linie = io.Source
    .fromResource("ogniem-i-mieczem.txt")
    .getLines.toList

  def histogram(maks: Int): String = {
    val text = linie.mkString(" ")
    val cleanedText = text.toLowerCase.filter(_.isLetter)
    val frequencies = cleanedText.groupBy(identity).view.mapValues(_.length).toMap
    val maxFrequency = frequencies.values.max

    frequencies.toSeq.sortBy(_._1).map { case (char, count) =>
      val barLength = (count.toDouble / maxFrequency * maks).toInt
      s"$char:${"*" * barLength}"
    }.mkString("\n")
  }

  val max = 50
  val result = histogram(max)
  println(result)
}