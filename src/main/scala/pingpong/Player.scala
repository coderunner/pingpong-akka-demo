package pingpong

import akka.actor.{Props, ActorRef, Actor}
import scala.util.Random
import pingpong.Game._
import pingpong.Game.BallMissedException
import pingpong.Game.Opponent

/**
 * Player Actor
 * 
 * @param name The name of the player
 */
class Player(name: String) extends Actor {

  /**
   * Initial state, wait for opponent!
   */
  override def receive = waitForOpponent

  /**
   * Wait for an opponent. Once the opponent is known, tell the sender we are ready to start!
   */
  def waitForOpponent: PartialFunction[Any, Unit] = {
    case Opponent(opponent) =>
      context.become(playAgainst(opponent))
      sender ! PlayerReady
  }

  /**
   * Play the game against the opponent!
   *
   * @param opponent The opponent actor
   */
  def playAgainst(opponent: ActorRef): PartialFunction[Any, Unit] = {
    case Ball =>
      if (Random.nextDouble() < 0.20) {
        throw new BallMissedException(exchangeWinner = sender, name)
      } else {
        opponent ! Ball
      }
    case Serve => opponent ! Ball
  }
}

object Player {
  def props(name: String) = Props(classOf[Player], name)
}
