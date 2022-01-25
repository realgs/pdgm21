package Kalaha

import javax.sound.midi.Receiver
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import concurrent.duration.DurationInt
import Kalaha.TypeOfPlayer.{Computer, Human, Player, RandomPlayer}

class Server {

  val time = 30
  var board = new Board
  var player1: Player = _
  var player2: Player = _

  def chooseGame(): Unit = {
    println("Wybierz rozgrywke: ")
    println("(1) uzytkownik - uzytkownik")
    println("(2) uzytkownik - komputer")
    println("(3) komputer - komputer")
    println("(4) random - random")
    println("(5) uzytkownik - random")
    println("(6) komputer - random")


    var choice = -1
    try {
      choice = scala.io.StdIn.readInt()
    } catch {
      case _: IllegalArgumentException => "wprowadzono zły rodzaj danych"
    }

    if (choice >= 1 && choice <= 6) {
      choice match {
        case 1 =>
          player1 = new Human(board)
          player2 = new Human(board)

        case 2 =>
          player1 = new Human(board)
          player2 = new Computer(board)

        case 3 =>
          player1 = new Computer(board)
          player2 = new Computer(board)

        case 4 =>
          player1 = new RandomPlayer(board)
          player2 = new RandomPlayer(board)

        case 5 =>
          player1 = new Human(board)
          player2 = new RandomPlayer(board)

        case 6 =>
          player1 = new Computer(board)
          player2 = new RandomPlayer(board)
      }

    } else {
      println("Bledny numer")
      chooseGame()
    }
  }

  def startGame(): Unit = {
    chooseGame()
    board.prepareGame()
    game()
  }

  var beginOfTheGame = true

  def game(): Unit = {

    if beginOfTheGame then
      board.printBoard()
      beginOfTheGame = false

    while (board.isNextMovePossibe()) {
      try {
        val moveTime = Future {
          if (board.firstPlayerMove && board.isNextMovePossibe()) {
            println("Gracz 1: ")
            val index = player1.move()
            board.makeMove(index)
            board.takeStonesFromOpositeHoleIfItIsPossible()
            board.printBoard()
            board.nextPlayer()
          } else if (!board.firstPlayerMove && board.isNextMovePossibe()) {
            println("Gracz 2: ")
            val index = player2.move()
            board.makeMove(index)
            board.takeStonesFromOpositeHoleIfItIsPossible()
            board.printBoard()
            board.nextPlayer()
          } else if (!board.isNextMovePossibe()) {
            endGame()
          }
        }
        Await.result(moveTime, time.second)
      } catch {
        case _: TimeoutException => println("Przekroczono limit czasu")
          return false
      }
    }
    endGame()

  }

  def endGame(): Unit = {
    board.endOfGame(board.firstPlayerMove)
    board.printResults()
  }
}

