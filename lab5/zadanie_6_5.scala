object SportCompetition {

  case class Ocena(imie: String, nazwisko: String, wdziek: Int, spryt: Int)
  case class Wynik(imie: String, nazwisko: String, sredniaWdziek: Double, sredniaSpryt: Double, suma: Double)

  def obliczWyniki(oceny: List[Ocena]): List[Wynik] = {
    val grupy = oceny.groupBy(o => (o.imie, o.nazwisko))

    val wyniki = for {
      ((imie, nazwisko), oceny) <- grupy.toList
      sredniaWdziek = oceny.map(_.wdziek).sum.toDouble / oceny.size
      sredniaSpryt = oceny.map(_.spryt).sum.toDouble / oceny.size
      suma = sredniaWdziek + sredniaSpryt
    } yield Wynik(imie, nazwisko, sredniaWdziek, sredniaSpryt, suma)

    wyniki.sortBy(w => (-w.suma, -w.sredniaWdziek, w.nazwisko, w.imie))
  }

}

@main
def mainSportCompetition(): Unit = {
  val oceny = List(
    SportCompetition.Ocena("Jan", "Kowalski", 15, 18),
    SportCompetition.Ocena("Jan", "Kowalski", 17, 16),
    SportCompetition.Ocena("Anna", "Nowak", 20, 19),
    SportCompetition.Ocena("Anna", "Nowak", 18, 20),
    SportCompetition.Ocena("Piotr", "Zielinski", 10, 12),
    SportCompetition.Ocena("Piotr", "Zielinski", 12, 14)
  )

  val wyniki = SportCompetition.obliczWyniki(oceny)

  wyniki.zipWithIndex.foreach { case (wynik, index) =>
    println(s"${index + 1}. ${wynik.imie} ${wynik.nazwisko} - Wdziek: ${wynik.sredniaWdziek}, Spryt: ${wynik.sredniaSpryt}, Suma: ${wynik.suma}")
  }
}
