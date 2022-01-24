package KalahaGame
import KalahaGame.*

import concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException, blocking}
import scala.util.Random

import java.awt.Robot
import java.awt.event.KeyEvent

class Server {

  var board = new Board
  var player1: Player = _
  var player2: Player = _
  private val robot = new Robot
  private var timeoutCounter = 0

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
        println("\nWrong input. Try again!\n")
        play()
    }
    makeMove(player1)
  }

  def makeMove(player: Player): Unit = {
    try{
        while(!board.isGameOver) {
          board.printBoard()
          val move = Future {
            player.makeMove(board)
          }
          val choice = Await.result(move, 30.seconds)
          timeoutCounter = 0
          if board.isPlayer1Move then println("\n1. player's choice: " + choice) else println("\n2. player's choice: " + choice)
          board.makeMove(choice)
          if board.hasLastPlayerOneMoreMove then {
            println("\nYou have one more move!\n")
            makeMove(player)
          }

          else if board.isPlayer1Move then makeMove(player1)
          else makeMove(player2)
      }
    } catch {
      case e: TimeoutException => {
        robot.keyPress(KeyEvent.VK_ENTER)
        println("\nTime out, try again")
        if (timeoutCounter == 3) {
//          board.printScoreAfterTimeOut()
          board.ifGameIsOver = true
          board.printScore()
        }
        timeoutCounter += 1
        makeMove(player)
      }
    }
  }
}
