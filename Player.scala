trait Player {
  var id: Int
  def requestMove(board: KalahaBoard): Int
  def notifyOfIllegalMovement(): Unit
  def sendResult(scores: (Int, Int)): Unit
}
