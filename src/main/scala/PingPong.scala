import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, Config}
import pingpong._
import pingpong.Game.StartGame

object PingPong {

  def main(args: Array[String]) {
    ConfigFactory.load()

    // Create the actor system
    val system = ActorSystem("PingPong")

    // Create the Game actor
    val game = system.actorOf(Game.props())

    // Start the match between the two pro players
    game ! StartGame("Jeff", "Félix")
  }
}
