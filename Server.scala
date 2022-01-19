import scala.util.Random
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.Random
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt

class Server {

  def playGame(player1: Player, player2: Player): Int =

    var game = new KalahaGame()
    var currentPlayer = player1

    println(s"The game is starting!")
    val playerToBegin = game.whoStarts()
    println(s"Player $playerToBegin begins")
    if playerToBegin == 2 then currentPlayer = player2

    while (game.gameStatus(currentPlayer) != -1)
      {
        try
        {
          val makeMove = Future {
            game.gameBoard.printBoard()
            var chosenCup = currentPlayer.makeMove(game)
            while (chosenCup < 0)
            {
              chosenCup = currentPlayer.makeMove(game)
            }
            game.makeMove(currentPlayer.getID(), chosenCup)
          }
          val res = Await.result(makeMove, 30.seconds)
          if res == 2 then {
            println("Captured!")
            if currentPlayer.getID() == 1 then currentPlayer = player2
            else currentPlayer = player1
          }
          else if res == 1 then println("Extra move!")
          else if currentPlayer.getID() == 1 then currentPlayer = player2
          else currentPlayer = player1
        }
        catch {
          case e: TimeoutException => println(s"Game over - Player ${currentPlayer.getID()} is not responding!")
            //game.gameResults()
            return -1
        }
      }
    game.gameResults()
    return 0
}
