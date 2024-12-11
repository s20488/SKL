import org.apache.pekko
import pekko.actor._
import scala.concurrent.duration._
import scala.util.Random

class Defender(castle: ActorRef) extends Actor {
  import Castle._

  override def preStart(): Unit = castle ! RegisterDefender(self)

  def receive: Receive = {
    case Fire(activeDefenders) =>
      val hitChance = activeDefenders.toDouble / (2 * 100)
      Random.nextDouble() match {
        case hit if hit <= hitChance =>
          println(s"${self.path.name} was hit")
          self ! PoisonPill
        case _ =>
          println(s"${self.path.name} dodged the shot")
      }
  }

  override def postStop(): Unit = castle ! DefenderDown(self)
}

class Castle(name: String) extends Actor {
  import Castle._
  import context._

  private var defenders = Set.empty[ActorRef]

  def receive: Receive = {
    case RegisterDefender(defender) =>
      defenders += defender
      watch(defender)

    case DefenderDown(defender) =>
      defenders -= defender
      println(s"$name: Defender ${defender.path.name} has fallen. Remaining defenders: ${defenders.size}")
      if (defenders.isEmpty) {
        println(s"$name has been defeated")
        context.system.terminate()
      }

    case Terminated(defender) =>
      defenders -= defender
      println(s"$name: Defender ${defender.path.name} terminated. Remaining defenders: ${defenders.size}")
      if (defenders.isEmpty) {
        println(s"$name has been defeated")
        context.system.terminate()
      }

    case FireCommand =>
      defenders.nonEmpty match {
        case true =>
          for (defender <- defenders) defender ! Fire(defenders.size)
        case false =>
          println(s"$name has no defenders left to fire")
      }
  }
}

object Castle {
  case object FireCommand
  case class Fire(activeDefenders: Int)
  case class RegisterDefender(defender: ActorRef)
  case class DefenderDown(defender: ActorRef)
}

class HigherForce(castles: List[ActorRef]) extends Actor {
  import Castle._
  import context._

  def receive: Receive = {
    case "start" =>
      context.system.scheduler.scheduleWithFixedDelay(
        Duration.Zero,
        1.second,
        self,
        "shoot"
      )(dispatcher)
    case "shoot" =>
      castles.foreach { castle =>
        castle ! FireCommand
      }
  }
}

@main
def battleSimulation(): Unit = {
  val system = ActorSystem("system")

  val castle1 = system.actorOf(Props(new Castle("castle1")), "castle1")
  val castle2 = system.actorOf(Props(new Castle("castle2")), "castle2")

  for (i <- 1 to 100) {
    system.actorOf(Props(new Defender(castle1)), s"castle1Defender$i")
    system.actorOf(Props(new Defender(castle2)), s"castle2Defender$i")
  }

  println("defenders created for both castles")

  val higherForce = system.actorOf(Props(new HigherForce(List(castle1, castle2))), "higherForce")

  higherForce ! "start"
}
