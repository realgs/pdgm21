package kalaha.actors

import akka.actor.{Actor, ActorRef, PoisonPill, Timers}
import akka.pattern.ask
import akka.util.Timeout
import kalaha.models.Connection.Move
import kalaha.models.PlayerEnum.{First, Player, Second}
import kalaha.models.{Actions, Connection, GameBoard, GameResult}
import kalaha.resource.strings.*
import kalaha.utils.Constants.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}


class Server extends Actor with Timers {

  private val players: Array[ActorRef] = new Array[ActorRef](2)

  private var gameBoard: GameBoard = _

  implicit private val timeout: Timeout = Timeout(TIME_LIMIT)

  override def receive: Receive = handleReceive(players)


 private def handleReceive(players: Array[ActorRef]): Receive = {
       
   case Connection.Connect(playerID: Int) =>
     players(playerID) = sender()
     println(s"Player ${playerID+1} has connected to the server")
     if(!players.contains(null)) then startGame()

   case Connection.Move(pitIndex: Int) =>
     sender ! gameBoard.moveStones(pitIndex)

   case Connection.Next =>
     gameBoard.printBoard()
     val move = players(gameBoard.getTurn().id) ? Actions.Move(gameBoard)

     move onComplete{
       case Success(pit: Int) =>
         boardMakeMove(pit)
       case Failure(_) =>
          println(timeout_info)
          handleTimeout()

     }

   case Connection.validateMove(pit: Int) =>
     sender ! gameBoard.isMoveValid(pit)


   case Connection.EndGame =>
     val result = gameBoard.getWinner()
     val PlayerScores = gameBoard.getScores()
     result match
       case GameResult.FirstWon =>
         println(first_player_won_text)
         println(s"First: ${PlayerScores._1}  :  Second: ${PlayerScores._2}")
       case GameResult.SecondWon =>
         println(second_player_won_text)
         println(s"Second: ${PlayerScores._2}  :  First: ${PlayerScores._1}")
       case GameResult.Draw =>
         println(draw_dialog_title)
         println(s" ${PlayerScores._1} : ${PlayerScores._2} ")

     end()




 }

  private def boardMakeMove(index: Int): Unit =
    gameBoard.moveStones(index)

    if gameBoard.isEndOfGame() then
      self ! Connection.EndGame
    else
      self ! Connection.Next

  private def handleTimeout(): Unit =
    gameBoard.switchTurns()
    self ! Connection.Next

  private def disconnectPlayers(): Unit =
    for(player <- players) {
      player ! PoisonPill
    }

  private def startGame(): Unit =
    gameBoard = new GameBoard()
    println(start_info)
    self ! Connection.Next

  private def end() =
    context.system.terminate()

}
