package kalahgame

trait Player:

  def pickMove(board: Board): Int
  def ID: Int
  def ID_=(newId: Int): Unit

end Player
