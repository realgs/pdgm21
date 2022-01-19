package model.gameModel

import model.decisionTreeModel.SingleDecision
import model.playerModel.Computer

class Simulation(private val player1: Computer, private val player2: Computer) extends Game(player1, player2):

  override def next(isFirstPlayerMove :Boolean): Unit =
    drawBoard(isFirstPlayerMove)
    val (indexOfSubTree, isEnding, nextMove, _): (Int, Boolean, Boolean, SingleDecision) = if isFirstPlayerMove then player1.makeMove() else player2.makeMove()
    if isFirstPlayerMove then player2.updateAfterEnemiesMove(indexOfSubTree) else player1.updateAfterEnemiesMove(indexOfSubTree)
//    if GameSpecification.AILEVEL < 7 then Thread.sleep(2000)
    if !isEnding then next(if nextMove then isFirstPlayerMove else !isFirstPlayerMove) else end()


