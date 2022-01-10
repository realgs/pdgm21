package player
import server.Server

// Most of the variables used here won't be needed for the human player
abstract class Player (protected var isFirstPlayer: Boolean) {
  def chooseMove(server: Server): Int
  def getIsFirstPlayer(): Boolean = isFirstPlayer
}
