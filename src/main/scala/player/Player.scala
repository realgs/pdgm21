package player
import board.KalahaBoard

// Most of the variables used here won't be needed for the human player
abstract class Player (firstPlayer: Boolean) {
  protected var isFirstPlayer: Boolean = firstPlayer

  def chooseMove(kalahaBoard: KalahaBoard): Int
}
