package Kalaha.Player

import Kalaha.KalahaAI.DecisionTree.*
import Kalaha.Player.Player.*

object SmartComputerPlayer {

  class SmartComputerPlayer(playerSymbol: String) extends Player() :
    private val decisionTree: DecisionTree = new DecisionTree(playerSymbol)

    override def getNextMove(): Int =
      decisionTree.calculateNextMove()

    override def makeMove(lastMove: Int): Unit =
      decisionTree.makeMove(lastMove)
}
