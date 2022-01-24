package kalahgame

import scala.annotation.tailrec

sealed trait BT[+A]:
  def pit: Int
case object Empty extends BT[Nothing]:
  def pit: Int = -1
case class Node[+A](board: A, pitNumber: Int, children: List[BT[A]], movingPlayer: Int) extends BT[A]:
  def pit: Int = pitNumber

class DecisionTree:

  def generateDecisionTree(board: Board, depth: Int, activePlayer: Int): BT[Board] =
    val children = generateChildren(board, depth, activePlayer)
    val root = new Node(board, -1,children, activePlayer)
    root

  def generateChildren(board: Board, depth: Int, activePlayer: Int): List[BT[Board]] =
    if depth == 0 then List(Empty)
    else
      val list: List[BT[Board]] =
        for(i <- List(0,1,2,3,4,5))
          yield
          val newBoard = board.copy()
          if(newBoard.validSelection(i, activePlayer)) then
            val bonusRound = newBoard.makePlay(i, activePlayer, false)
            if(bonusRound) then
              Node(newBoard, i, generateChildren(newBoard, depth-1, activePlayer), activePlayer)
            else
              if activePlayer == 1 then
                Node(newBoard, i, generateChildren(newBoard, depth-1, 2), 2)
              else
                Node(newBoard, i, generateChildren(newBoard, depth-1, 1), 1)
          else
            Empty
      if (list == List(Empty, Empty, Empty, Empty, Empty, Empty)) then
        List(Empty)
      else
        list

end DecisionTree
