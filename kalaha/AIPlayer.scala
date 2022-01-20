package kalaha

import scala.util.Random

class AIPlayer(var holeMin: Int, var holeMax: Int, var server: Server) extends Player {
  var base: Int = holeMax
  var NUMBER_OF_HOLES: Int = 14
  var lastHole: Int = 0

  def makeMove(): List[Int] =
    var bestMove = getBestMove
    var requests = List(bestMove)
    while (server.requestMove(bestMove) == 1) {
      if !server.isOver then
        bestMove = getBestMove
        requests = requests ::: List(bestMove)
    }
    requests

  def checkIfNotEmpty(hole: Int): Boolean =
    server.getBoardArray(hole) == 0

  def getCurrentBoard: Array[Int] =
    server.getBoardArray.clone()

  def simulateMovement(hole: Int, board: Array[Int]): (Array[Int]) =
    var index = (hole + 1) % NUMBER_OF_HOLES
    while (board(hole) != 0) {
      board(hole) -= 1
      board(index) += 1
      index = (index + 1) % NUMBER_OF_HOLES
    }
    board

  def getDifferenceMax(board: Array[Int]): Int =
    board.slice(holeMin, holeMax + 1).sum - board.slice(holeMax + 1 % NUMBER_OF_HOLES, NUMBER_OF_HOLES - holeMin).sum

  def minimax(position: Int, depth: Int, maximizingPlayer: Boolean): Node =
    if depth == 0 || isGameOverSimulation(simulateMovement(position, getCurrentBoard)) then
      new Node(getDifferenceMax(simulateMovement(position, getCurrentBoard)))
    else if maximizingPlayer then
      val maxEval = new Node(Int.MinValue)
      for (i <- holeMin to holeMax) {
        val eval = minimax(i, depth - 1, false)
        maxEval.value = math.max(maxEval.value, eval.value)
        maxEval.children :+ eval
      }
      maxEval
    else
      val minEval = new Node(Int.MaxValue)
      for (i <- holeMin to holeMax) {
        val eval = minimax(i, depth - 1, true)
        minEval.value = math.min(minEval.value, eval.value)
        minEval.children :+ eval
      }
      minEval


  def isGameOverSimulation(board: Array[Int]): Boolean =
    board.slice(0, 6).sum == 0 || board.slice(7, 13).sum == 0

  def getBestMove: Int =
    var bestHole = -1
    var maxVal = Node(Int.MinValue)
    for (i <- holeMin until holeMax) {
      if !checkIfNotEmpty(i) then
        val value = minimax(i, 5, true)
        maxVal = if (maxVal.value <= value.value) value else maxVal
        bestHole = if (maxVal.value == value.value) i else bestHole
    }
    bestHole + 1
}

class Node(var value: Int = 0) {
  var children: Array[Node] = Array.empty
}
