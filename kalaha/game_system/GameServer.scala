package game_system

import board.Board
import players.*

import scala.io.StdIn.readInt

class GameServer {

  import GameServer.STONES_PER_HOUSE
  import GameServer.HOUSE_NR

  def start(): Unit =
    printMenu()
    initializeGame()

  private def printMenu(): Unit =
    println("Welcome in Kalaha game!")
    println("In this game each player has 30 seconds to move. After that time player loses.")
    println("How do you want to play?")
    println("1. Player vs Player")
    println("2. Player vs AI")
    println("3. show AI vs AI")

  private def initializeGame(): Unit =
    val gameService = new GameService()
    val board = new Board(HOUSE_NR, STONES_PER_HOUSE)
    var isCorrectChoice = false

    while !isCorrectChoice do
      readUserInput() match
        case 1 =>
          gameService.initialize(board, new HumanPlayer(1), new HumanPlayer(2))
          isCorrectChoice = true
        case 2 =>
          gameService.initialize(board, new HumanPlayer(1), new AIPlayer(2))
          isCorrectChoice = true
        case 3 =>
          gameService.initialize(board, new AIPlayer(1), new AIPlayer(2))
          isCorrectChoice = true
        case _ =>
          println("Incorrect. Try again")
          isCorrectChoice = false

  private def readUserInput(): Int =
    print("Your choice: ")
    var userChoice = -1

    try {
      userChoice = readInt()
    }catch {
      case e: IllegalArgumentException => println("That's not an integer number.")
      case _ => println("Something wrong.")
    }
    userChoice

}

object GameServer{
  final val HOUSE_NR = 6
  final val STONES_PER_HOUSE = 4
  final val TIME_PER_PLAYER_MOVE = 20
}
