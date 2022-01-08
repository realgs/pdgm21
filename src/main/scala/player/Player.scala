package player
import board.KalahaBoard

// Most of the variables used here won't be needed for the human player
abstract class Player {
  private var isFirstPlayer: Boolean = _

  def initialize(isFirstPlayer: Boolean) =
    this.isFirstPlayer = isFirstPlayer

  def chooseMove(board: KalahaBoard): Int
}
