package players

import akka.actor.TypedActor.dispatcher
import akka.actor.{Actor, PoisonPill, actorRef2Scala}
import game.GameServer.MoveChoice
import players.Player.{GameOver, MoveRequest}

object Player{
  case class MoveRequest()
  case class GameOver()
}

abstract class Player(val playerNum: Int) extends Actor{

  def receive = {
    case MoveRequest => sender() ! MoveChoice(playerNum, chooseMove())
    case GameOver => self ! PoisonPill
  }

  def chooseMove(): Int
}
