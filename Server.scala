package KalahaGame
import KalahaGame.*

import concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException, blocking}
import scala.util.Random

class Server {

  var board = new Board
  var player1: Player = _
  var player2: Player = _

  def start(): Unit = {
    play()
    makeMove(player1)
  }

  def printMenu(): Unit = {
    println("Menu")
    println("Options:")
    println("1. player vs player")
    println("2. player vs computer")
    println("3. computer vs computer")
    print("Selected option: ")
  }

  def tryAgain(player: Player): Unit = {
    println("Wrong input. Try again.")
    makeMove(player)
  }

  private def play(): Unit = {
    printMenu()
    var choice = 0
    try {
      choice = scala.io.StdIn.readInt()
    } catch {
      case _: Exception => choice = 0
    }
    choice match {
      case 1 =>
        player1 = new HumanPlayer()
        player2 = new HumanPlayer()
      case 2 =>
        player1 = new HumanPlayer()
        player2 = new Computer()
      case 3 =>
        player1 = new Computer()
        player2 = new Computer()
      case _ =>
        println("Wrong input. Try again!\n")
        play()
    }
    makeMove(player1)
  }

  def makeMove(player: Player): Unit = {
    try{
      val move = Future {
        while(!board.isGameOver) {
          board.printBoard()
          val choice = player.makeMove(board)
          if board.if1PlayerMove then println("\n1. player's choice: " + choice) else println("\n2. player's choice: " + choice)
          board.makeMove(choice)
          if board.ifOneMoreMove then makeMove(player)
          else if board.isPlayer1Move then makeMove(player1)
          else makeMove(player2)
        }
      }
      Await.result(move, 20.seconds)
    } catch {
      case e: TimeoutException => println("Time out")
        board.ifGameIsOver = true
        board.printScore()
    }
  }
}
