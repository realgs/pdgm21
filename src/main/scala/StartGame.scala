import gameboard.KalahaBoard
import server.Server
import player.HumanPlayer
import player.AIPlayer

object StartGame {
  def main(args: Array[String]): Unit = {
    val server = Server()
    server.initializeGame(6, 5, true, true)
    server.playGame()
  }
}
