import Kalaha.*
import Player.*
import UserPlayer.*
import ComputerBestPlayer.*
import ComputerRandomPlayer.*
import scala.io.StdIn.*

class Server {
  private var board: Kalaha = new Kalaha()
  private var player1: Player = _
  private var player2: Player = _

  def runGame: Unit =
    println("Choose player 1 (1 - 3).\n" +
      "1 - User\n" +
      "2 - Computer (random moves)\n" +
      "3 - Computer (best moves)")
    var option = readInt()
    while(option < 1 || option > 3)
      println("Input was incorrect. Choose number from 1 to 3.")
      option = readInt()
    option match
      case 1 => player1 = new UserPlayer()
      case 2 => player1 = new ComputerRandomPlayer()
      case 3 => player1 = new ComputerBestPlayer(1)

    println("Choose player 2 (1 - 3).\n" +
      "1 - User\n" +
      "2 - Computer (random moves)\n" +
      "3 - Computer (best moves)")
    option = readInt()
    while(option < 1 || option > 3)
      println("Input was incorrect. Choose number from 1 to 3.")
      option = readInt()
    option match
      case 1 => player2 = new UserPlayer()
      case 2 => player2 = new ComputerRandomPlayer()
      case 3 => player2 = new ComputerBestPlayer(2)

    println("\nPlayer " + board.getWhoseTurn + " starts!")
    while (!board.checkGameOver)
      board.printBoard
      makeMove

    board.printBoard
    board.printResults

  def makeMove: Unit =
    if board.getWhoseTurn == 1 then
      board.playerMakesMove(player1.chooseNextMove(board))
    else board.playerMakesMove(player2.chooseNextMove(board))
}
