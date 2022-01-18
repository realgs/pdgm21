trait Player:
  var id: Int
  def requestMove(board: KalahaBoard): Int
  def notifyOfIllegalMovement(): Unit
  def notifyOfGameInterrupted(playerWonId: Int): Unit
  def sendResult(scores: (Int, Int)): Unit

