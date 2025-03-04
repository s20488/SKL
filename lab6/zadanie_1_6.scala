import akka.actor._

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
  ActorSystem("PingPongSystem").actorOf(Props(new Actor {
    override def preStart(): Unit = {
      context.actorOf(PropsPlayer, "player1") ! Start(
        context.actorOf(PropsPlayer, "player2")
      )
    }
    def receive: Receive = Actor.emptyBehavior
  }))
}
