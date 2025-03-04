import org.apache.pekko
import pekko.actor._

case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case class Wykonaj(tekst: String, id: Int)
case class Wynik(unikalneSlowa: Int, id: Int)

class Boss extends Actor {
  def receive: Receive = {
    case "Dzien dobry" =>
      println("Boss odebrał wiadomość: Dzień dobry")

    case Init(liczbaPracownikow) =>
      (1 to liczbaPracownikow).map(_ => context.actorOf(PropsPracownik)).toList match {
        case pracownicy =>
          println(s"Boss zainicjował $liczbaPracownikow pracowników.")
          context.become(withWorkers(pracownicy, List(), Map(), 0))
      }
  }
  def withWorkers(pracownicy: List[ActorRef], oczekujaceZlecenia: List[String], wyniki: Map[Int, Int], oczekujaceOdpowiedzi: Int): Receive = {
    case Zlecenie(tekst) =>
      println(s"Boss otrzymał zlecenie na przetworzenie ${tekst.size} linii tekstu.")
      tekst.take(pracownicy.size).zipWithIndex.foreach { case (linia, idx) =>
        pracownicy(idx) ! Wykonaj(linia, idx)
      }
      context.become(withWorkers(pracownicy, tekst.drop(pracownicy.size), Map(), pracownicy.size))

    case Wynik(unikalneSlowa, id) =>
      println(s"Boss odebrał wynik od pracownika $id: $unikalneSlowa unikalnych słów.")
      val noweWyniki = wyniki + (id -> unikalneSlowa)
      val noweOczekujaceOdpowiedzi = oczekujaceOdpowiedzi - 1

      if (oczekujaceZlecenia.nonEmpty) {
        val linia = oczekujaceZlecenia.head
        sender() ! Wykonaj(linia, id)
        context.become(withWorkers(pracownicy, oczekujaceZlecenia.tail, noweWyniki, noweOczekujaceOdpowiedzi + 1))
      } else if (noweOczekujaceOdpowiedzi == 0) {
        val suma = noweWyniki.values.sum
        println(s"Całkowita liczba unikalnych słów: $suma")
      } else {
        context.become(withWorkers(pracownicy, oczekujaceZlecenia, noweWyniki, noweOczekujaceOdpowiedzi))
      }
  }
}

class Pracownik extends Actor {
  def receive: Receive = {
    case Wykonaj(tekst, id) =>
      sender() ! Wynik(
        tekst.split("\\s+")
          .map(_.toLowerCase.replaceAll("[^a-zA-Z0-9]", ""))
          .filter(_.nonEmpty)
          .distinct
          .length,
        id
      )
  }
}

@main
def zad4: Unit = {
  ActorSystem("WordCounter").actorOf(Props(new Actor {
    override def preStart(): Unit = {
      context.actorOf(PropsBoss, "boss") ! "Dzien dobry"
      context.actorOf(PropsBoss, "boss") ! Init(3)
      context.actorOf(PropsBoss, "boss") ! Zlecenie(
        scala.io.Source.fromResource("ogniem_i_mieczem.txt").getLines.toList
      )
    }
    def receive: Receive = Actor.emptyBehavior
  }))
}
