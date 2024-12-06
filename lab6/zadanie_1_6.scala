import org.apache.pekko.actor._

case class Start(next: ActorRef)
case object Ping
case object Pong

class Player extends Actor {
  def receive: Receive = {
    case Start(next) =>
      println(s"${self.path.name} zaczyna grę z ${next.path.name}")
      next ! Ping

    case Ping =>
      println(s"${self.path.name}: Otrzymałem 'Ping', odpowiadam 'Pong'")
      sender() ! Pong

    case Pong =>
      println(s"${self.path.name}: Otrzymałem 'Pong', odpowiadam 'Ping'")
      sender() ! Ping
  }
}

@main
def zad1(): Unit = {
  val system = ActorSystem("PingPongSystem")

  println("Rozpoczynamy grę w ping-ponga dla dwóch graczy.")
  val player1 = system.actorOf(Props[Player](), "player1")
  val player2 = system.actorOf(Props[Player](), "player2")
  player1 ! Start(player2)

  //  println("\nRozpoczynamy grę w ping-ponga dla trzech graczy.")
  //  val playerA = system.actorOf(Props[Player](), "playerA")
  //  val playerB = system.actorOf(Props[Player](), "playerB")
  //  val playerC = system.actorOf(Props[Player](), "playerC")
  //  playerA ! Start(playerB)
  //  playerB ! Start(playerC)
  //  playerC ! Start(playerA)
  //
  //  println("\nRozpoczynamy grę w ping-ponga dla listy graczy.")
  //  val players = (1 to 5).map(i => system.actorOf(Props[Player](), s"player_$i")).toList
  //  for (i <- players.indices) {
  //    players(i) ! Start(players((i + 1) % players.size))
  //  }
}
