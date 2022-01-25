package Kalaha.TypeOfPlayer

import Kalaha.Board

import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Computer(val board: Board) extends Player {
  val depth = 3

  override def move(): Int = {
    val move = bestMove(board)
    println(s"Komputer - numer pola : $move")
    return move
  }

  def bestMove(board: Board): Int = {
    var bestChoice = -1
    var highestScore = Int.MinValue
    var listFutures: List[Future[Int]] = List()

    for (i <- 0 to 12) {
      if (board.checkCorrectnessOfField(i)) {
        val newBoard = board.cloneBoard()
        newBoard.makeMove(i)
        newBoard.nextPlayer()
        val advantage = checkPossibleMoves(newBoard, depth, board.firstPlayerMove == newBoard.firstPlayerMove, board.firstPlayerMove)

        if (advantage > highestScore || bestChoice == -1) {
          highestScore = advantage
          bestChoice = i

        }
      }
    }

    return bestChoice
  }


  def checkPossibleMoves(board: Board, depth: Int, better: Boolean, player: Boolean): Int = {
    var currentAdvantage = Int.MinValue

    if (depth == 0) return board.countDifference(board.firstPlayerMove)
    if (!board.isNextMovePossibe()) {
      board.endOfGame(board.firstPlayerMove)
      if (board.countDifference(player) > 0) {
        return Int.MaxValue
      }
      else return Int.MinValue
    }
    if (better) {

      for (i <- 0 to 12) {
        if (board.checkCorrectnessOfField(i)) {
          val newboard = board.cloneBoard()
          newboard.makeMove(i)
          newboard.nextPlayer()
          val advantage = checkPossibleMoves(newboard, depth - 1, player == newboard.firstPlayerMove, player)
          currentAdvantage = currentAdvantage.max(advantage)
        }
      }
    }
    else {
      currentAdvantage = Int.MaxValue
      for (i <- 0 to 12) {
        if (board.checkCorrectnessOfField(i)) {
          val newboard = board.cloneBoard()
          newboard.makeMove(i)
          newboard.nextPlayer()
          val advantage = checkPossibleMoves(newboard, depth - 1, player == newboard.firstPlayerMove, player)
          currentAdvantage = currentAdvantage.min(advantage)

        }
      }
    }
    currentAdvantage
  }
}
