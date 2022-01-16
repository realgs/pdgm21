package Kalaha.KalahaAI

import Kalaha.Board.KalahaBoard.*
import Kalaha.KalahaAI.Node.*

object DecisionTree {

  class DecisionTree(playerSymbol: String):
    private var root: Node = new Node(new KalahaBoard(), -1, playerSymbol, "A")
    root.calculateTree(9, true)

    def getNextMove(): Int =
      root.calculateTree(9, false)
      root.calculateBestOutcome()

      root.bestMove

    def makeMove(lastMove: Int): Unit =
      root.calculateTree(9, false)
      root = root.nextMoves(lastMove)
}
