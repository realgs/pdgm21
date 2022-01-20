import Kalaha.*
import Player.*

object ComputerBestPlayer {
  class ComputerBestPlayer(val number: Int) extends Player {
    override def chooseNextMove(board: Kalaha): Int =
      val decisionTree = createDecisionTree(board, 8)
      val pitNumber = minimax(decisionTree, true)._2
      println("Player " + board.getWhoseTurn + " chose pit number " + pitNumber)
      pitNumber

    def getOpponentNumber: Int =
      if number == 1 then 2
      else 1

    sealed trait DecisionTree
    case object Empty extends DecisionTree
    //Empty - the move is impossible or it's the end of the tree
    case class Node(board: Kalaha, whoseTurn: Int, additionalMove: Boolean, rootMove: Int, moves: Array[DecisionTree]) extends DecisionTree
    //board - current board state in the simulation
    //whoseTurn - who makes next move
    //additionalMove - did moving here give the player additional move?
    //rootMove - first move made in the tree
    //moves - all next moves

    def getWhoseTurnFromNode(decisionTree: DecisionTree): Int =
      decisionTree match
        case Empty => 0
        case Node(_, playerTurn, _, _, _) => playerTurn

    def createDecisionTree(boardState: Kalaha, depth: Int): DecisionTree =
      def createDecisionTreeInner(simulationBoard: Kalaha, currentDepth: Int, rootMove: Int, nextMove: Int): DecisionTree =
        currentDepth match
          case 0 => Empty
          case _ =>
            val nextSimulationBoard = simulationBoard.copyBoard

            //if that next move is possible
            if nextSimulationBoard.checkCorrectPitNumber(nextMove) then
              val additionalMove = nextSimulationBoard.playerMakesMove(nextMove)
              if nextSimulationBoard.checkGameOver then
                Node(nextSimulationBoard, 0, additionalMove, rootMove,
                  Array[DecisionTree](Empty, Empty, Empty, Empty, Empty, Empty))
              else
                Node(nextSimulationBoard, nextSimulationBoard.getWhoseTurn, additionalMove, rootMove,
                  Array[DecisionTree](createDecisionTreeInner(nextSimulationBoard, currentDepth - 1, rootMove, 1),
                    createDecisionTreeInner(nextSimulationBoard, currentDepth - 1, rootMove, 2),
                    createDecisionTreeInner(nextSimulationBoard, currentDepth - 1, rootMove, 3),
                    createDecisionTreeInner(nextSimulationBoard, currentDepth - 1, rootMove, 4),
                    createDecisionTreeInner(nextSimulationBoard, currentDepth - 1, rootMove, 5),
                    createDecisionTreeInner(nextSimulationBoard, currentDepth - 1, rootMove, 6)))

            //if that next move is impossible
            else Empty

      Node(boardState, boardState.getWhoseTurn, false, 0,
        Array[DecisionTree](createDecisionTreeInner(boardState, depth, 1, 1),
          createDecisionTreeInner(boardState, depth, 2, 2),
          createDecisionTreeInner(boardState, depth, 3, 3),
          createDecisionTreeInner(boardState, depth, 4, 4),
          createDecisionTreeInner(boardState, depth, 5, 5),
          createDecisionTreeInner(boardState, depth, 6, 6)))


    //returns highest possible difference between players' scores and number of the move that leads to it
    //maximizingPlayer - if it's the turn of the player who is looking for the highest difference
    def minimax(tree: DecisionTree, maximizingPlayer: Boolean): (Int, Int, Boolean) =
      tree match
        //it's the last calculated node
        case Node(currentBoard, _, additionalMove, rootMove, Array(Empty, Empty, Empty, Empty, Empty, Empty)) =>
          (currentBoard.getPlayerScore(number) - currentBoard.getPlayerScore(getOpponentNumber), rootMove, additionalMove)

        //the node has children (calculated next moves)
        case Node(_, _, _, _, moves) =>
          if maximizingPlayer then
            var maxEval = Integer.MIN_VALUE
            var bestRootMove = 0
            var additionalMove = false
            for (i <- 0 to 5)
            //checking if the move is possible
              if moves(i) != Empty then
                val eval = minimax(moves(i), getWhoseTurnFromNode(moves(i)) == number)
                if eval._1 > maxEval || (eval._1 == maxEval && eval._3) then
                  maxEval = eval._1
                  bestRootMove = eval._2
                  additionalMove = eval._3
            (maxEval, bestRootMove, additionalMove)
          else
            var minEval = Integer.MAX_VALUE
            var bestRootMove = 0
            var additionalMove = false
            for (i <- 0 to 5)
              if moves(i) != Empty then
                val eval = minimax(moves(i), getWhoseTurnFromNode(moves(i)) == number)
                if eval._1 <= minEval || (eval._1 == minEval && eval._3) then
                  minEval = eval._1
                  bestRootMove = eval._2
                  additionalMove = eval._3
            (minEval, bestRootMove, additionalMove)
  }
}
