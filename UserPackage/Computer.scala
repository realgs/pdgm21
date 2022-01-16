package com.example.UserPackage

import com.example.DecisionPackage.DecisionDifferenceTree
import com.example.GamePackage.Game
import javax.swing.JTextPane

class Computer(private val game: Game, private val gamePane: JTextPane, private var enemy: Player = null) extends Player(game, gamePane) {

  override def setEnemy(user2: Player): Unit = {
    enemy = user2
  }

  override def moveRequest(board: Game): Unit = {
    val decisionTree = new DecisionDifferenceTree(board)
    decisionTree.makeTree()
    Thread.sleep(600)
    server.moveReceived(enemy, decisionTree.findMaxScoreHole())
  }
}