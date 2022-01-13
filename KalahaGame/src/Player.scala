import GameServer.MoveChoice
import akka.actor.{Actor, PoisonPill, actorRef2Scala}
import Player.{FinalScore, MoveRequest}
import akka.actor.TypedActor.dispatcher

object Player{
  case class MoveRequest()
  case class FinalScore(p1: Int, p2: Int)
}

abstract class Player(val playerNum: Int) extends Actor{
  def receive = {
    case MoveRequest => sender() ! MoveChoice(playerNum, chooseMove())
    case FinalScore(p1: Int, p2: Int) =>
      displayMyScore(p1, p2)
      self ! PoisonPill
  }

  def chooseMove(): Int

  def displayMyScore(p1: Int, p2: Int): Unit ={
    playerNum match{
      case 1 => println("Player 1 score: " + p1)
      case 2 => println("Player 2 score: " + p2)
    }
  }

}
