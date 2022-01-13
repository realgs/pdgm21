import GameServer.{MoveChoice, NextMove, StartGame}
import Player.{FinalScore, MoveRequest}
import ServerTests.system
import akka.actor.{Actor, ActorRef, Kill, PoisonPill, Props, actorRef2Scala}
import akka.pattern.*

import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}
import akka.util.Timeout

import scala.io.StdIn.readInt


class GameServer(private var player1: ActorRef, private var player2: ActorRef, private val board: Board) extends Actor{
  var currentPlayer = 1
  var nextMove = false
  implicit private val timeout: Timeout = Timeout(5.seconds)



  def receive = {
    case StartGame =>
      printGameBoard()
      requestMove()

    case NextMove => requestMove()
//      if checkIfPossible(playerNum, choice) then {
//        printPlayersChoiceMessage(choice)
//        var nextMove = board.move(playerNum, choice)
//        if gameFinished() then endGame()
//        else if nextMove then {
//          printNextMoveMessage()
//          printGameBoard()
//            sender() ! MoveRequest
//        } else{
//          changeCurrentPlayer()
//          printGameBoard()
//          printTurnMessage()
//          getCurrentPlayer() ! MoveRequest
//        }
//      } else
//        println("This move is not possible")
//        sender() ! MoveRequest
  }

  def requestMove(): Unit ={
    printTurnMessage()
    val f = getCurrentPlayer() ? MoveRequest
    f.onComplete{
      case Success(MoveChoice(pNum, choice)) =>
        if checkIfPossible(pNum, choice) then
          makeMove(pNum, choice)
        else
          println("This move is not possible")
          requestMove()

      case Failure(ex) =>
        val randMove = getRandomMove(currentPlayer)

        if currentPlayer == 1 then
          system.stop(player1)
          //player1 =  system.actorOf(Props(HumanPlayer(1)))
        else
          system.stop(player2)
          //player2 =  system.actorOf(Props(HumanPlayer(2)))

        println("Time is up, making random move: $randMove")
        makeMove(currentPlayer, randMove)
    }
  }

  def makeMove(playerNum: Int, house: Int): Unit ={
    printPlayersChoiceMessage(house)
    nextMove = board.move(playerNum, house)
    if gameFinished() then endGame()
    else
      if nextMove then {
        printNextMoveMessage()
        printGameBoard()
      }else{
        changeCurrentPlayer()
        printGameBoard()
      }
    self ! NextMove
  }

  def getRandomMove(playerNum: Int): Int = {
    val rand = new Random()
    var choice = rand.nextInt(5) + 1
    while(!checkIfPossible(playerNum, choice)){
      choice = rand.nextInt(5) + 1
    }
    choice
  }

  def checkIfPossible(playerNum: Int, house: Int): Boolean ={
    playerNum match{
      case 1 => board.player1houses(house - 1) > 0
      case 2 => board.player2houses(house - 1) > 0
    }
  }

  def printTurnMessage(): Unit ={
    println(s"Player $currentPlayer your turn")
  }
  def printNextMoveMessage(): Unit ={
    println(s"Player $currentPlayer has next move")
  }
  def printPlayersChoiceMessage(choice: Int): Unit ={
    println(s"Player $currentPlayer choice: $choice")
  }

  def getCurrentPlayer(): ActorRef ={
    if currentPlayer == 1 then player1
    else player2
  }

  def changeCurrentPlayer(): Unit ={
    if currentPlayer == 1 then currentPlayer = 2
    else currentPlayer = 1
  }

  def printGameBoard(): Unit = {
    board.printBoard()
  }

  def gameFinished(): Boolean = {
    board.isFinished()
  }


  def endGame(): Unit ={
    val (s1, s2): (Int, Int) = board.getFinalScore()
    player1 ! FinalScore(s1, s2)
    player2 ! FinalScore(s1, s2)
    self ! PoisonPill

    context.system.terminate()
  }
}

object GameServer{
  case class MoveChoice(val playerNum: Int, val choice: Int)
  case class StartGame()
  case class NextMove()
}


