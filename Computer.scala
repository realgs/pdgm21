package KalahaGame
import KalahaGame._


class Computer extends Player {
  def makeMove(board: Board): Int = {
    Thread.sleep(800)
    getBestResultFieldNumber(createResults(board))
  }


  def getBestResultFieldNumber(results: Array[Int]): Int = {
    var bestResultFieldNumber = 0
    var bestResult = results(0)

    for(i <- 1 to 5) {
      if(results(i) > bestResult) {
        bestResult = results(i)
        bestResultFieldNumber = i
      }
    }
    bestResultFieldNumber + 1
  }

  def createResults(board: Board): Array[Int] = {
    val results: Array[Int] = new Array(6)
    for(i <- 1 to 6)
      results(i - 1) = countResult(i, board.clone())
    results
  }

  def countResult(fieldNumber: Int, board: Board): Int = {
    //if(!board.ifCorrectFieldNumber(fieldNumber)) return Int.MinValue
    val ifPlayer1Move = board.isPlayer1Move
    board.makeMove(fieldNumber)
    val scoresAfterMove = board.getScores
    var resultAfterMove = scoresAfterMove._2 - scoresAfterMove._1
    if(ifPlayer1Move) resultAfterMove = -resultAfterMove
    if(board.hasLastPlayerOneMoreMove) resultAfterMove + getBestResultFieldNumber(createResults(board))
    else resultAfterMove
  }
}
