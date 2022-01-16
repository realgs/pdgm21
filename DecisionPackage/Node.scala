package com.example.DecisionPackage

import com.example.GamePackage.Game

class Node(private val board: Game, private val playerNumber: Int, private val level: Int, private val parent: Node, private val fieldChoice: Int) {
  private val PLAYER_1 = 1
  private val PLAYER_2 = 2
  private val INDEX1_PLAYER1 = 0
  private val INDEX2_PLAYER2 = 7
  private val HOLE_PLAYER1 = 6
  private val HOLE2_PLAYER2 = 13

  private var children: List[Node] = Nil

  private val difference =
    if (board.getActivePlayerNumber == playerNumber)
      board.countPlayerResultsDifference()
    else {
      board.changeActivePlayer()
      val result = board.countPlayerResultsDifference()
      board.changeActivePlayer()
      result
    }


  def getChildren(): List[Node] = {
    children
  }

  def getParent(): Node = {
    parent
  }

  def getDifference(): Int = {
    difference
  }

  def getFieldChoice(): Int = {
    fieldChoice
  }
  def getLevel(): Int = {
    level
  }

  def findNodeChildren(): List[Node] = {
    def findNodeChildrenInner(currentIndex: Int): List[Node] =
      if ((board.getActivePlayerNumber == PLAYER_1 && (currentIndex >= HOLE_PLAYER1 || currentIndex < INDEX1_PLAYER1)) || (board.getActivePlayerNumber == PLAYER_2 && (currentIndex >= HOLE2_PLAYER2 || currentIndex < INDEX2_PLAYER2))) Nil
      else {
        if (!board.checkIfEnd() && board.getBoardHole(currentIndex) != 0) {
          val testBoard = board.cloneBoard()
          testBoard.processPlayerMove(currentIndex)
          val child = new Node(testBoard, playerNumber, level + 1, this, currentIndex)
          child :: findNodeChildrenInner(currentIndex + 1)
        } else {
          if (currentIndex + 1 == HOLE_PLAYER1 || currentIndex + 1 >= HOLE2_PLAYER2) Nil
          else findNodeChildrenInner(currentIndex + 1)
        }
      }
    children = findNodeChildrenInner(if (board.getActivePlayerNumber == PLAYER_1) INDEX1_PLAYER1 else INDEX2_PLAYER2)
    children
  }
}
