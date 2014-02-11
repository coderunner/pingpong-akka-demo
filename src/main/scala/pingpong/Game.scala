package pingpong

import akka.actor._
import akka.actor.SupervisorStrategy.Resume
import akka.actor.OneForOneStrategy

/**
 * Actor representing a game. It supervise the two players and keep the score.
 */
class Game extends Actor with ActorLogging {
  import Game._

  /**
   * Supervision strategy, normally used to handle actor failures.
   *
   * In this case, the failure means that the player missed the ball and the opponent marks a point.
   * The losing player is then resumed for the game to continue.
   */
  override val supervisorStrategy =
    OneForOneStrategy() {
      case BallMissedException(player, _) =>
        self ! Point(player)
        Resume
    }

  /**
   * Initial state, wait for the game to start!
   */
  override def receive: Actor.Receive = waitForGameStart

  /**
   * Wait until we get a StartGame message with the two players' name.
   */
  def waitForGameStart: PartialFunction[Any, Unit] = {
    case StartGame(player1Name, player2Name) =>
      // Create the two players actor. Since we created them, we will supervise them.
      val player1 = context.actorOf(Player.props(player1Name))
      val player2 = context.actorOf(Player.props(player2Name))

      // Give each player his opponent reference.
      player1 ! Opponent(player2)
      player2 ! Opponent(player1)

      // Wait for both players to be ready...
      context.become(waitForPlayerReady(Map(player1 -> player1Name, player2 -> player2Name), Set()))
  }

  /**
   * Wait for players to be ready.
   */
  def waitForPlayerReady(playerNames: Map[ActorRef, String], playersReady: Set[ActorRef]): PartialFunction[Any, Unit] = {
    case PlayerReady =>
      log.info("Player " + playerNames(sender) + " ready!")
      val ready = playersReady + sender

      // Check if both players are ready.
      if(ready.size == 2) {
        // If yes, change to playGame mode.
        context.become(playGame(playerNames, ready.map(p => p -> 0).toMap))
        // One player serves!
        sender ! Serve
      } else {
        // Not all players are ready, wait...
        context.become(waitForPlayerReady(playerNames, Set(sender)))
      }
  }

  /**
   * Play!
   */
  def playGame(playerNames: Map[ActorRef, String], score: Map[ActorRef, Int]): PartialFunction[Any, Unit] = {

    // A player made a point!
    case Point(playerWhoMarkedAPoint) =>
      log.info("Point for " + playerNames(playerWhoMarkedAPoint))

      // Update the score
      val newScore = score + (playerWhoMarkedAPoint -> (score(playerWhoMarkedAPoint) + 1))

      // Check if there is a winner
      val maybeWinner = newScore.find(s => s._2 == 21)
      maybeWinner match {
        case None =>
          // No winner keep playing.
          log.info(newScore.map(entry => playerNames(entry._1) + " : " + entry._2).mkString(" - "))
          context.become(playGame(playerNames, newScore))
          playerWhoMarkedAPoint ! Serve
        case Some((winner, _)) =>
          log.info(playerNames(winner) + " wins!")
          // Stop players.
          playerNames.foreach(entry => context.stop(entry._1))
      }
  }
}

object Game {

  // The ball!
  case object Ball

  // Message to tell a player to server the ball.
  case object Serve

  // Tell a player who he/she is playing against.
  case class Opponent(opponent: ActorRef)

  // Start the game message.
  case class StartGame(player1: String, player2: String)

  // Send by a player once he/she is ready.
  case object PlayerReady

  // A player marked a point.
  case class Point(player: ActorRef)

  // A player missed the ball.
  case class BallMissedException(exchangeWinner: ActorRef, exchangeLoser: String)
    extends Exception(exchangeLoser + " missed!")

  def props() = Props(classOf[Game])
}
