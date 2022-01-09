import gameboard.KalahaBoard
import server.Server
import player.HumanPlayer
import player.AIPlayer

object StartGame {
  def main(args: Array[String]): Unit = {
    var test = Server()
    test.initializeGame(6, 5, false, false)
    test.makeMovePlayer()
    test.makeMovePlayer()
    test.makeMovePlayer()
  }
}
