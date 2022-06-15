package game

import akka.actor.{Actor, ActorRef, Kill, PoisonPill, Props, actorRef2Scala}
import akka.pattern.*
import akka.util.Timeout
import game.Board
import game.GameServer.{MoveChoice, NextMove, StartGame}
import KalahaGame.system
import players.*
import Player.{MoveRequest, GameOver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.io.StdIn.readInt
import scala.util.{Failure, Random, Success}



class GameServer(player1Mode: Int, player2Mode: Int) extends Actor{
  val board = new Board()
  var currentPlayer = 1
  var nextMove = false
  implicit private val timeout: Timeout = Timeout(30.seconds)
  val player1 = {
    if player1Mode == 1 then system.actorOf(Props(HumanPlayer(1)), "player1")
    else  system.actorOf(Props(Computer(1, board)), "player1")
  }

  val player2 = {
    if player2Mode == 1 then system.actorOf(Props(HumanPlayer(2)), "player2")
    else system.actorOf(Props(Computer(2, board)), "player2")
  }

  def receive = {
    case StartGame =>
      printGameBoard()
      requestMove()
    case NextMove => requestMove()
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
        printEndMessage()
        changeCurrentPlayer()
        walkoverForPlayer(currentPlayer)
    }
  }

  def makeMove(playerNum: Int, house: Int): Unit ={
    printPlayersChoiceMessage(house)
    Thread.sleep(100)
    nextMove = board.move(playerNum, house)
    if gameFinished() then
      printGameBoard()
      printEndMessage()
      printFinalScore()
      endGame()
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

  def printEndMessage(): Unit ={
    println("\n-------------------GAME OVER--------------------\n")
  }


  def endGame(): Unit ={
    player1 ! GameOver
    player2 ! GameOver
    self ! PoisonPill
    context.system.terminate()
    System.exit(0)
  }

  def printFinalScore(): Unit ={
    val (s1, s2): (Int, Int) = board.getFinalScore()
    println("                  Final score                   ")
    println(s"  Player 1: $s1                  Player 2: $s2 ")
    if s1 > s2 then
      println("             Player 1 is the winner!")
    else if s2 > s1 then
      println("             Player 2 is the winner!")
    else println("It's a draw!")
    println()
  }

  def walkoverForPlayer(playerNum: Int): Unit ={
    println("                    Walkover!")
    println(s"                 Player $playerNum wins!\n")
    endGame()
  }
}

object GameServer{
  case class MoveChoice(val playerNum: Int, val choice: Int)
  case class StartGame()
  case class NextMove()
}
