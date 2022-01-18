package server
import akka.actor.{Actor, ActorRef, Kill, PoisonPill, Props, actorRef2Scala}
import akka.pattern.*
import akka.util.Timeout
import gameUtensils.KalahaBoard
import javax.swing.JLabel
import server.Server.{PlayersMove,  GameOn, GameOff, Walkover}
import player.Player
import player.Player.{MoveStones, GameEnd}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}
import server.ServerCommunication.system

object Server
{
  case class GameOn()
  case class PlayersMove(val playersID: Int, val pit: Int)
  case class Walkover()
  case class GameOff()
}

class Server(private val kalahaBoard: KalahaBoard, private val player1: ActorRef, private val player2: ActorRef) extends Actor
{
  implicit private val timeout: Timeout = Timeout(40.seconds)

  println("Legend")
  println(kalahaBoard.indexLegend())

  def receive =
    case GameOn => askPlayerAboutMove()
    case GameOff => gameOffActions()
    case Walkover => walkoverActions()

  def askPlayerAboutMove(): Unit =
    println("\nCurrent state of board: ")
    println(kalahaBoard.showBoard())
    println(s"\nPlayer's ${kalahaBoard.getActivePlayer()} turn!")

    val player: ActorRef = whoIsPlaying()
    val f = player ? MoveStones

    f.onComplete {
      case Success(PlayersMove(playerID, pitIndex)) =>
        if kalahaBoard.isMoveCorrect(pitIndex) then
          if kalahaBoard.performMoveOfOnePlayer(pitIndex) then self ! GameOff else self ! GameOn
        else {println("That's not a correct move!"); self ! Walkover}

      case Failure(exception) =>
        println("You didn't make a move in requested time!")
        system.stop(player)
        self ! Walkover
    }

  def walkoverActions(): Unit =
    println("\nWalkover!")

    if kalahaBoard.getActivePlayer() == 1 then println("Player 2 won!")
    else println("Player 1 won!")

    endGame()

  def gameOffActions(): Unit =
    println("\nGame is finished!")
    println(kalahaBoard.showScore())
    println(kalahaBoard.showWinningPLayer())

    endGame()

  def endGame(): Unit =
    player1 ! GameEnd
    player2 ! GameEnd
    self ! PoisonPill
    println("\n\nGoodbye!")
    System.exit(0)

  def whoIsPlaying(): ActorRef =
    if kalahaBoard.getActivePlayer() == 1 then player1
    else player2

}
