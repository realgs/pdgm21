import scala.io.StdIn

class HumanConsolePlayer extends Player:
  var id: Int = _
  def messageColor = if id == 1 then Console.GREEN else if id == 2 then Console.BLUE else Console.WHITE
  def log(message: String) = println(s"$messageColor Player$id: $message ${Console.RESET}")
  override def notifyOfIllegalMovement(): Unit =
    log("This move is illegal, try again")

  override def notifyOfGameInterrupted(playerWonId: Int): Unit =
    if playerWonId != id then
      log("You have ran out of time, you lost!")
    else
      log("your opponent has ran out of time, you won!")

  override def sendResult(scores: (Int, Int)): Unit =
    val (player1Score, player2Score) = scores
    val scoreDiffrence = player1Score - player2Score
    if scoreDiffrence == 0 then
      log("Draw!")
    else
      val playerWon = if scoreDiffrence > 0 then 1 else 2
      if playerWon == this.id then
        log("You won! c:")
      else
        log("You lost! :c")

      log(s"player1: $player1Score - player2: $player2Score")

  override def requestMove(board: KalahaBoard): Int =
    board.printBoard()
    log(s"Choose a house: (0-${board.housesPerSide - 1})")
    StdIn.readInt()

