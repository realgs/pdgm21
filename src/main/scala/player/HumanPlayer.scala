package player
import board.KalahaBoard

class HumanPlayer (firstPlayer: Boolean) extends Player (firstPlayer) {
  // Human players start indexing at 1 not 0
  override def chooseMove(kalahaBoard: KalahaBoard): Int =
    return 1
}
