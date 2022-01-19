package server

import akka.actor.*
import akka.util.Timeout
import client.Player
import game.Board

import scala.concurrent.duration._

object ServerManager {

  case class start()
  case class moveTakenFromPlayer(pitNumber: Int)
}

class ServerManager(val firstPlayer: ActorRef, val secondPlayer: ActorRef, val board: Board) extends Actor with Timers {

  var currentPlayer = firstPlayer
  var otherPlayer = secondPlayer

  val timeout: Duration = 30.seconds
  context.setReceiveTimeout(timeout)

  override def receive: Receive = {

    case ServerManager.start =>
      board.initializeBoard()
      board.displayBoard()
      currentPlayer ! Player.moveRequest(board)

    case ServerManager.moveTakenFromPlayer(pitNumber) =>
      println("Chosen pit is: " + pitNumber)
      makeMove(pitNumber)

    case _ =>
      terminateGame()
  }

  def makeMove(pitNumber: Int): Unit =

    var i = pitNumber + 1

    if (board.isChosenPitCorrect(pitNumber))

      var amountOfStones = board.array(pitNumber)
      board.array(pitNumber) = 0

      if (board.isFirstPlayerTurn)
        while (amountOfStones != 0)
          if (i == 13)
            i = 0
          board.array(i) += 1
          i += 1
          amountOfStones -= 1

        if (board.array(i - 1) == 1 && (i - 1) >= 0 && (i - 1) <= 5 && (board.array(12 - (i - 1)) != 0))
          board.array(6) += (board.array(i - 1) + board.array(12 - (i - 1)))
          board.array(i - 1) = 0
          board.array(12 - (i - 1)) = 0

        if (i != 7)
          board.isFirstPlayerTurn = !board.isFirstPlayerTurn
          swapPlayer()

      else

        while (amountOfStones != 0)
          if (i == 14)
            i = 0
          else if (i == 6)
            i = 7
          board.array(i) += 1
          i += 1
          amountOfStones -= 1

        if (board.array(i - 1) == 1 && (i - 1) >= 7 && (i - 1) <= 12 && (board.array(12 - (i - 1)) != 0))
          board.array(13) += (board.array(i - 1) + board.array(12 - (i - 1)))
          board.array(i - 1) = 0
          board.array(12 - (i - 1)) = 0

        if (i != 14)
          board.isFirstPlayerTurn = !board.isFirstPlayerTurn
          swapPlayer()

    board.displayBoard()

    if (isGameEnded())
      whoIsTheWinner()
      context.system.terminate()
      return

    currentPlayer ! Player.moveRequest(board)


  def swapPlayer(): Unit =
    var temp = currentPlayer
    currentPlayer = otherPlayer
    otherPlayer = temp

  def isGameEnded(): Boolean =
    
    var counter = 0
    
    for(i <- 0 to 5)
      if (board.array(i) == 0)
        counter += 1
    if (counter == 6)
      return true
    counter = 0
    for (i <- 7 to 12)
      if (board.array(i) == 0)
        counter += 1
    if (counter == 6)
      return true
    return false  
    

  def whoIsTheWinner(): Unit =

    var pointsOfFirstPlayer = board.array(6)
    var pointsOfSecondPlayer = board.array(13)

    for(i <- 0 to 5)
      pointsOfFirstPlayer += board.array(i)

    for(i <- 7 to 12)
      pointsOfSecondPlayer += board.array(i)

    if (pointsOfFirstPlayer > pointsOfSecondPlayer)
      println(Console.GREEN + "First player won the game !")
    else if (pointsOfFirstPlayer < pointsOfSecondPlayer)
      println(Console.GREEN + "Second player won the game !")
    else
      println(Console.GREEN + "Draw !")

  def terminateGame() =
    if (otherPlayer == firstPlayer)
      println(Console.GREEN + "First player won the game !")
    else if (otherPlayer == secondPlayer)
      println(Console.GREEN + "Second player won the game !")
    context.system.terminate()

}
