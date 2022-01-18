package player
import akka.actor.{Actor, PoisonPill, actorRef2Scala}
import Player.{MoveStones, GameEnd}
import server.Server.PlayersMove

object Player
{
  case class MoveStones()
  case class GameEnd()
}

abstract class Player(val playerID: Int) extends Actor
{
  def receive =
    case MoveStones => sender() ! PlayersMove(playerID, makeMove())
    case GameEnd => self ! PoisonPill

  def makeMove(): Int
}
