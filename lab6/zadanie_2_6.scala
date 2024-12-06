import org.apache.pekko
import pekko.actor._

case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case class Wykonaj(tekst: String, id: Int)
case class Wynik(unikalneSlowa: Int, id: Int)

class Boss extends Actor {
  private var pracownicy: List[ActorRef] = List()
  private var oczekujaceZlecenia: List[String] = List()
  private var wyniki: Map[Int, Int] = Map()
  private var oczekujaceOdpowiedzi: Int = 0

  def receive: Receive = {
    case "Dzien dobry" =>
      println("Boss odebrał wiadomość: Dzień dobry")

    case Init(liczbaPracownikow) =>
      pracownicy = (1 to liczbaPracownikow).map(_ => context.actorOf(Props[Pracownik]())).toList
      println(s"Boss zainicjował $liczbaPracownikow pracowników.")

    case Zlecenie(tekst) =>
      println(s"Boss otrzymał zlecenie na przetworzenie ${tekst.size} linii tekstu.")
      oczekujaceZlecenia = tekst
      wyniki = Map()
      oczekujaceOdpowiedzi = pracownicy.size

      for ((linia, idx) <- tekst.take(pracownicy.size).zipWithIndex) {
        pracownicy(idx) ! Wykonaj(linia, idx)
      }
      oczekujaceZlecenia = oczekujaceZlecenia.drop(pracownicy.size)

    case Wynik(unikalneSlowa, id) =>
      println(s"Boss odebrał wynik od pracownika $id: $unikalneSlowa unikalnych słów.")
      wyniki += (id -> unikalneSlowa)
      oczekujaceOdpowiedzi -= 1

      if (oczekujaceZlecenia.nonEmpty) {
        val linia = oczekujaceZlecenia.head
        oczekujaceZlecenia = oczekujaceZlecenia.tail
        sender() ! Wykonaj(linia, id)
        oczekujaceOdpowiedzi += 1
      } else if (oczekujaceOdpowiedzi == 0) {
        val suma = wyniki.values.sum
        println(s"Całkowita liczba unikalnych słów: $suma")
      }
  }
}

class Pracownik extends Actor {
  def receive: Receive = {
    case Wykonaj(tekst, id) =>
      val unikalneSlowa = tekst
        .split("\\s+")
        .map(_.toLowerCase.replaceAll("[^a-zA-Z0-9]", ""))
        .filter(_.nonEmpty)
        .distinct
        .length
      sender() ! Wynik(unikalneSlowa, id)
  }
}

@main
def zad4: Unit = {

  def dane(): List[String] = {
    scala.io.Source.fromResource("ogniem_i_mieczem.txt").getLines.toList
  }

  val system = ActorSystem("WordCounter")
  val boss = system.actorOf(Props[Boss](), "boss")
  boss ! "Dzien dobry"
  boss ! Init(3)
  boss ! Zlecenie(dane())

  println(dane())
}
