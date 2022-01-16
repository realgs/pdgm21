package com.example.DecisionPackage

import com.example.GamePackage.Game
import scala.annotation.tailrec

class DecisionDifferenceTree(var game: Game) {

  private val root = new Node(game.cloneBoard(), game.getActivePlayerNumber,0,null,-1)

  def makeTree(): Unit = {
    @tailrec
    def makeTreeInner(decisionTree: List[Node]): Unit =
      decisionTree match {
        case node :: nextNodes => if (node.getLevel() < 2) makeTreeInner(nextNodes ::: node.findNodeChildren()) else ()
        case Nil => ()
      }
    makeTreeInner(List(root))
  }

  def findMaxScoreHole(): Int = {
    @tailrec
    def findMaxScoreHoleInner(decisionTree: List[Node], currentBestNode: Node): Node =
      decisionTree match {
        case Nil => currentBestNode
        case (currentNode :: nextNodes) => if (currentNode.getChildren == Nil && currentNode.getDifference > currentBestNode.getDifference) findMaxScoreHoleInner(nextNodes, currentNode)
                                           else if (currentNode.getChildren == Nil) findMaxScoreHoleInner(nextNodes, currentBestNode)
                                           else findMaxScoreHoleInner(nextNodes ::: currentNode.getChildren, currentBestNode)

      }
    var decisionNode = findMaxScoreHoleInner(List(root), root.getChildren().head)
    while (decisionNode.getParent() != root) decisionNode = decisionNode.getParent()
    decisionNode.getFieldChoice()
  }
}
