@main
def zad5: Unit = {
  case class Województwo(nazwa: String, min: Int)
  // max ID gminy z województwa w: w.min + 19999
  case class Wynik(
                    ID: Int,
                    KOALICJA_OBYWATELSKA: Int,
                    LEWICA_RAZEM: Int,
                    POLEXIT: Int,
                    JEDNOŚĆ_NARODU: Int,
                    PIS: Int,
                    EUROPA_CHRISTI: Int,
                    WIOSNA: Int,
                    KONFEDERACJA: Int,
                    KUKIZ15: Int,
                    POLSKA_FAIR_PLAY: Int
                  )

  val województwa = List(
    Województwo("dolnośląskie", 20000),
    Województwo("kujawsko-pomorskie", 40000),
    Województwo("lubelskie", 60000),
    Województwo("lubuskie", 80000),
    Województwo("łódzkie", 100000),
    Województwo("małopolskie", 120000),
    Województwo("mazowieckie", 140000),
    Województwo("opolskie", 160000),
    Województwo("podkarpackie", 180000),
    Województwo("podlaskie", 200000),
    Województwo("pomorskie", 220000),
    Województwo("śląskie", 240000),
    Województwo("świętokrzyskie", 260000),
    Województwo("warmińsko-mazurskie", 280000),
    Województwo("wielkopolskie", 300000),
    Województwo("zachodniopomorskie", 320000)
  )

  val wyniki = io.Source
    .fromResource("wyniki.csv")
    .getLines
    .toList
    .map(l => {
      l.split(",").toList.map(_.toInt) match {
        case List(a, b, c, d, e, f, g, h, i, j, k) => Wynik(a, b, c, d, e, f, g, h, i, j, k)
        case _ => throw new IllegalArgumentException
      }
    })

  val wojewodztwoWyniki = województwa.map { woj =>
    val wynikiWoj = wyniki.filter(w => w.ID >= woj.min && w.ID <= woj.min + 19999)
    val sumaKO = wynikiWoj.map(_.KOALICJA_OBYWATELSKA).sum
    val sumaPiS = wynikiWoj.map(_.PIS).sum
    val roznica = math.abs(sumaKO - sumaPiS).toDouble / (sumaKO + sumaPiS) * 100
    (woj.nazwa, roznica)
  }

  val minRoznica = wojewodztwoWyniki.minBy(_._2)
  val minimalneRoznice = wojewodztwoWyniki.filter(_._2 == minRoznica._2)

  minimalneRoznice.foreach { case (nazwa, roznica) =>
    println(s"Wojewodztwo: $nazwa, Roznica procentowa: $roznica%")
  }
}
