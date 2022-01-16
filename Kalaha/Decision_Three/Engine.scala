package Kalaha.Decision_Three

import Kalaha.Gameboard.Game_board
import Kalaha.Server.Server

class Engine(val player: Int, val depth: Int, val server: Server) {
  var tree: Decision_tree = new Decision_tree
  val opponent: Int = (player + 1) % 2
  var board = new Game_board
  board.houses = server.game_board.cloneBoard()


  def best_move(): Int =
    tree_on(depth, tree.root, board, player, player)
    find_best_path()


  def tree_on(depth: Int, node: Node, board: Game_board, player: Int, player_no_change: Int): Unit =

    if (depth > 0)
      var valid_moves = board.valid_moves(player)
      node.children = new Array(board.NUMBER_HOUSES)

      for (i <- node.children.indices) {
        if (player == player_no_change) {
          node.children(i) = new Node(-200)
        }
          else node.children(i) = new Node(200)
      }


      for (_ <- valid_moves)
        val index = valid_moves.head
        if valid_moves.nonEmpty then valid_moves = valid_moves.tail
        val (board_after_move, rep) = board.board_after_theoretical_move(board.cloneBoard(), index, player)
        val player_advantage = board.calculate_point_diff(board_after_move, player)
        node.children(index) = new Node(player_advantage)
        val game = new Game_board
        game.houses = board_after_move
        if rep then tree_on(depth - 1, node.children(index), game, player, player_no_change)
        else tree_on(depth - 1, node.children(index), game, (player + 1) % 2, player_no_change)



  private def find_best_path(): Int = {
    var max: Int = -1
    var max_index: Int = 0
    for (i <- tree.root.children.indices) {
      val value = search(tree.root.children(i), depth - 1, player)._1
      val is_greater = search(tree.root.children(i), depth - 1, player)._2
      if (max < value && is_greater) {
        max = value
        max_index = i
      }

    }
    max_index
  }

  private def search(node: Node, depth: Int, opponent: Int): (Int, Boolean) = {
    if (depth == 0)
      (node.vale(), true)
    else {
      if (opponent == player) {
        var max: Int = 0
        if node.children == null then return (-200, false)

        for (i <- node.children.indices) {
          val value = search(node.children(i), depth - 1, opponent)._1 + node.vale()
          if (max < value)
            max = value

        }
        (max, true)
      }
      else {
        var min: Int = 200
        for (i <- node.children.indices) {
          val value = search(node.children(i), depth - 1, player)._1 + node.vale()
          if (min > value)
            min = value
        }
        (min, true)
      }
    }
  }


  class Node(value: Int) {

    def this() =
      this(-200)

    var children: Array[Node] = _

    def vale(): Int =
      value


  }

  class Decision_tree {
    var root: Node = new Node


  }
}

