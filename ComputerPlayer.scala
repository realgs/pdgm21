import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class ComputerPlayer extends Player:
  var id: Int = _

  def messageColor = if id == 1 then Console.GREEN else if id == 2 then Console.BLUE else Console.WHITE

  def log(message: String) = println(s"$messageColor ComPlayer$id: $message ${Console.RESET}")

  def opponentId = if id == 1 then 2 else 1

  override def notifyOfIllegalMovement(): Unit = ???

  override def notifyOfGameInterrupted(playerWonId: Int): Unit = log(":)")

  override def sendResult(scores: (Int, Int)): Unit =
    val (player1Scores, player2Scores) = scores
    println(s"Player1: $player1Scores, Player2: $player2Scores")

  def boardStaticEvaluation(board: KalahaBoard) =
    val (player1Scores, player2Scores) = board.scores
    val scoreDiff = player1Scores - player2Scores
    if id == 1 then scoreDiff else -scoreDiff

  def minimax(board: KalahaBoard, depth: Int, playerId: Int): Int =
    if depth == 0 || board.endGame() then
      boardStaticEvaluation(board)
    else
      if playerId == id then
        var maxEval = Integer.MIN_VALUE
        for(i <- 0 until board.housesPerSide if board.isMoveLegal(playerId, i))
          val nextBoard = board.copy()
          val nextPlayer = nextBoard.makeAMove(playerId, i)
          val eval = minimax(nextBoard, depth - 1, nextPlayer)
          if eval > maxEval then maxEval = eval

        maxEval
      else
        var minEval = Integer.MAX_VALUE
        for(i <- 0 until board.housesPerSide if board.isMoveLegal(playerId, i))
          val nextBoard = board.copy()
          val nextPlayer = nextBoard.makeAMove(playerId, i)
          val eval = minimax(nextBoard, depth - 1, nextPlayer)
          if eval < minEval then minEval = eval

        minEval

  override def requestMove(board: KalahaBoard): Int =
    val futures = for(i <- 0 until board.housesPerSide if board.isMoveLegal(id, i)) yield Future {
      val nextBoard = board.copy()
      val nextPlayer = nextBoard.makeAMove(id, i)
      (i, minimax(nextBoard, 10, nextPlayer))
    }
    val scores = Await.result(Future.sequence(futures), Duration.Inf)
//    val scores = futures
    val move = scores.maxBy(_._2)._1
    log(s"moving $move")
    move
