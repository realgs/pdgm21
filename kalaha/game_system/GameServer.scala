package game_system

import board.Board
import GameParameters.*
import players.*

import scala.io.StdIn.readInt

class GameServer {

  def start(): Unit =
    printMenu()
    initializeGame()

  private def printMenu(): Unit =
    println("Welcome in Kalaha game!")
    println("In this game each player has " + TIME_PER_PLAYER_MOVE + " seconds to move. After that time player loses.")
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
          gameService.initialize(board, new HumanPlayer(PlayerID.first), new HumanPlayer(PlayerID.second))
          isCorrectChoice = true
        case 2 =>
          gameService.initialize(board, new HumanPlayer(PlayerID.first), new AIPlayer(PlayerID.second))
          isCorrectChoice = true
        case 3 =>
          gameService.initialize(board, new AIPlayer(PlayerID.first), new AIPlayer(PlayerID.second))
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
