package kalahgame

class MinimaxDecision:

 /* def breadthBT (tree: BT[Board]): List[(Board,Int, List[BT[Board]])] =
    def breadthTraversal(childQueue: List[BT[Board]]): List[(Board,Int,  List[BT[Board]])] =
      childQueue match
        case Nil => Nil
        case Empty :: t =>
          //println("\n\n******************Empty********************\n\n")
          breadthTraversal(t)
        case Node(elem, pitNumber, children, movingPlayer)::t =>
         // println(s"\n\n******************$pitNumber********************")
          elem.printBoard()
          //println(s"*********************$movingPlayer*****************\n\n")
          (elem, pitNumber, children)::breadthTraversal (t ::: children)
    breadthTraversal (List(tree))*/

  def getBestMove(decisionTree:BT[Board], maximizingPlayer: Int):(Int, Int) =
    decisionTree match
      case Node(board, pitNumber, List(Empty), movingPlayer) =>
        (board.calcAdvantage(maximizingPlayer), pitNumber)
      case Node(board, pitNumber, children, movingPlayer) =>
        if(movingPlayer == maximizingPlayer)
          var max = -Int.MaxValue
          var index = -1
          for(i <- children.filter(_ != Empty))
            val actual = getBestMove(i, maximizingPlayer)._1
            if(max<actual) then
              max = actual
              index = i.pit
          (max, index)
        else
          var min = Int.MaxValue
          var index = -1
          for(i <- children.filter(_ != Empty))
            val actual = getBestMove(i, maximizingPlayer)._1
            if(min>actual) then
              min = actual
              index = i.pit
          (min, index)

end MinimaxDecision
