import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


class AIPlayer(first: Boolean) extends Player(first){
  val depth = 4

  override def makeMove(board: Board): Int =

    var listOfFutures: List[Future[(Int, Int)]] = List()
    var maxDifference = Integer.MIN_VALUE
    var bestMove = -1

    board.possibleMoves(first).foreach(
      house => {
        listOfFutures = Future {
          val newBoard = board.clone()
          val code = newBoard.move(house, first)

          if code == 1 then (minMaxAlgorithm(newBoard, depth, true), house)   //additional move
          else (minMaxAlgorithm(newBoard, depth - 1, false), house)
        } :: listOfFutures
      }
    )


    listOfFutures.foreach(
      future  => {
        val (difference, move): (Int, Int) = Await.result(future, Duration.Inf)
        if (difference > maxDifference || (difference == maxDifference && move > bestMove)) {
          maxDifference = difference
          bestMove = move
        }
      }
    )

    println(bestMove)
    bestMove



  /*
    isEndOfGame(first == maximizingPlayer)
    first - true, maximizingPlayer - true => isEndOfGame(true) player1 should move
    first - true, maximizingPlayer - false => isEndOfGame(false) player2 should move
    first - false, maximizingPlayer - true => isEndOfGame(false) player2 should move
    first - false, maximizingPlayer - false => isEndOfGame(true) player1 should move
  */

  def minMaxAlgorithm(board: Board, depth: Int, maximizingPlayer: Boolean): Int =
    if depth == 0 then board.countDifference(first)
    else if board.isEndOfGame(first == maximizingPlayer) then
      board.endOfGame(first == maximizingPlayer)
      board.countDifference(first)


    else if maximizingPlayer then
      var maxDifference = Integer.MIN_VALUE
      var bestMove = 0

      board.possibleMoves(first).foreach(
        house => {
          val newBoard = board.clone()
          val code = newBoard.move(house, first)
          var difference = 0

          if code == 1 then difference = minMaxAlgorithm(newBoard, depth, true)
          else difference = minMaxAlgorithm(newBoard, depth - 1, false)


          if (difference > maxDifference) {
            maxDifference = difference
            bestMove = house
          }
        }
      )
      maxDifference

    else
      var minDifference = Integer.MAX_VALUE
      var bestMove = 0

      board.possibleMoves(!first).foreach(
        house => {
          val newBoard = board.clone()
          val code = newBoard.move(house, !first)
          var difference = 0

          if code == 1 then difference = minMaxAlgorithm(newBoard, depth, false)
          else difference = minMaxAlgorithm(newBoard, depth - 1, true)


          if (difference < minDifference) {
            minDifference = difference
            bestMove = house
          }
        }
      )
      minDifference




}
