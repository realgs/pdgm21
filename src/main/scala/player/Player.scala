package player
import server.Server

// Most of the variables used here won't be needed for the human player
abstract class Player (protected var isFirstPlayer: Boolean, protected var name: String) {
  def getIsFirstPlayer(): Boolean = isFirstPlayer
  def getName(): String = name
}
