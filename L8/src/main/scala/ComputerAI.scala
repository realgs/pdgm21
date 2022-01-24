import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


class ComputerAI (private var enemyPlayer : Player = Player()) extends Player {

  class Node (var children : Array[Node], var diff : Int, var board : Board, val move : Int, val playerChanged : Boolean) {
  }

  var tree : Array[Node] = initTree()

  override def makeMove(): Int = {
    if !canMakeMove() then Int.MinValue
    else {
      tree = makeTree(initTree(), 9)
      var max = Int.MinValue
      var move = 1
      val futures = for (i <- tree.indices) yield Future {
          minimax(tree(i), true)
      }
      futures.map(Await.result(_, Duration.Inf))
      for (node <- tree) {
//        print(node.diff)
//        print(" ")
        if max < node.diff then {
          move = node.move
          max = node.diff
        }
      }
      print(" " + move)
      Thread.sleep(1000)
      move
    }
  }

  def initTree() : Array[Node] = {
    var tree : Array[Node] = Array()
    for( i <- 0 until 6) {
      val currentPlayer : Player = super.makeCopy()
      val enemyPlayer : Player = this.enemyPlayer.makeCopy()
      val board = Board(currentPlayer, enemyPlayer)
      if board.getCurrentPlayer().isPossible(i+1) then {
        val returned = board.makeMove(i + 1)
        if(board.isGameFinished()) {
          currentPlayer.gameEnd()
          enemyPlayer.gameEnd()
        }
        if returned == -1 then {
          tree = tree.appended(Node(Array(), currentPlayer.getBase - enemyPlayer.getBase, board.makeCopy(), i + 1, false))
        }
        else {
          tree = tree.appended(Node(Array(), currentPlayer.getBase - enemyPlayer.getBase, board.makeCopy(), i + 1, true))
        }
      }
    }
    tree
  }

  def makeTree(nodes : Array[Node], depth : Int) : Array[Node] = {
    if depth == 0 then Array()
    else {
      for (node <- nodes) {
        for (i <- 0 until 6) {
          if node.board.getCurrentPlayer().isPossible(i + 1) then {
            val thisPlayer = node.board.getPlayer1()
            val enemyPlayer = node.board.getPlayer2()

            val returned = node.board.makeMove(i + 1)
            if(node.board.isGameFinished()) {
              thisPlayer.gameEnd()
              enemyPlayer.gameEnd()
            }
            if returned == -1 then {
              node.children = node.children.appended(Node(Array(), thisPlayer.getBase - enemyPlayer.getBase, node.board.makeCopy(), i + 1, false))
            }
            else {
              node.children = node.children.appended(Node(Array(), thisPlayer.getBase - enemyPlayer.getBase, node.board.makeCopy(), i + 1, true))
            }
          }
        }
      }
      for (node <- nodes) {
        makeTree(node.children, depth - 1)
      }
      nodes
    }
  }

  def minimax(node : Node, maximazing : Boolean) : Int = {
    if node.children sameElements Array[Node]() then {
      node.diff
    }
    else if maximazing then {
      var max = Int.MinValue
      for(child <- node.children) {
        var eval = 0
        if node.playerChanged then {
          eval = minimax(child, false)
        }
        else {
          eval = minimax(child, true)
        }
        max = Math.max(max, eval)
      }
      node.diff = max
      max
    }
    else {
      var min = Int.MaxValue
      for(child <- node.children) {
        var eval = 0
        if node.playerChanged then {
          eval = minimax(child, true)
        }
        else {
          eval = minimax(child, false)
        }
        min = Math.min(min, eval)
      }
      node.diff = min
      min
    }
  }
  
  def setEnemyPlayer(player : Player) : Unit = {
    enemyPlayer = player
  }

}
