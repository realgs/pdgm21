import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeoutException


class Server(val player1: Player, val player2: Player) {

  private val board = Board()
  var currentPlayer: Player = player1


  def changePlayer(): Unit =
    if currentPlayer == player1 then currentPlayer = player2
    else currentPlayer = player1


  def play(): Unit =

    board.printBoard()
    var code = 3

    while !board.isEndOfGame(currentPlayer.first) do

      /*

      Codes returned by def move:
      0 => next player moves
      1 => this player has additional move
      2 => wrong move

      */

      code match
        case 1 => println("You have an additional move")
        case 2 => println("Wrong number of house")
        case _ => ()

      if currentPlayer.first then print("Player1's move: ")
      else print("Player2's move: ")

      var house = -1

      try {
        val fHouse = Future {
          currentPlayer.makeMove(board)
        }
        house = Await.result(fHouse, 30.seconds)
      }
      catch {

        case e: TimeoutException =>
          println()
          println("Time for the move is over")
          if currentPlayer.first then
            println("Because of the lack of activity of Player1 the game is over.")
            println("Player2 won by walk-over !!! ")
          else
            println("Because of the lack of activity of Player2 the game is over.")
            println("Player1 won by walk-over !!! ")

          System.exit(0)
      }

      code = board.move(house, currentPlayer.first)
      if (code == 0)
        changePlayer()
      board.printBoard()


    board.endOfGame(currentPlayer.first)
    board.printWhoWin()

}

