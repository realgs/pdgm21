import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class ComputerPlayer extends Player:
  var id: Int = _

  override def notifyOfIllegalMovement(): Unit = ???

  override def notifyOfGameInterrupted(playerWonId: Int): Unit = ???

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
        for(i <- 0 until board.housesPerSide)
          val nextBoard = board.copy()
          val nextPlayer = nextBoard.makeAMove(playerId, i)
          val eval = minimax(nextBoard, depth - 1, nextPlayer)
          if eval > maxEval then maxEval = eval

        maxEval
      else
        var minEval = Integer.MAX_VALUE
        for(i <- 0 until board.housesPerSide)
          val nextBoard = board.copy()
          val nextPlayer = nextBoard.makeAMove(playerId, i)
          val eval = minimax(nextBoard, depth - 1, nextPlayer)
          if eval < minEval then minEval = eval

        minEval

  override def requestMove(board: KalahaBoard): Int =
    val futures = for(i <- 0 until board.housesPerSide) yield Future {
      minimax(board, 5, id)
    }
    val scores = Await.result(Future.sequence(futures), Duration.Inf)
    scores.zipWithIndex.maxBy(_._1)._2
