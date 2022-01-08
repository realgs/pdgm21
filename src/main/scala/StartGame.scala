import board.KalahaBoard
import server.Server
import player.HumanPlayer

object StartGame {
  def main(args: Array[String]): Unit = {
    var test = Server()
    test.initializeGame(6, 5, new HumanPlayer(), new HumanPlayer())
    test.makeMovePlayer()
  }
}
