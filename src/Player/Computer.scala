package Player

import Player.*
import Human.*
import Computer.*
import Game.Game.*

import scala.annotation.tailrec
import scala.util.Random

object Computer {
  class Computer(game: Game, val AI: Boolean, DEPTH: Int) extends Player(game) {

    def randomHole(): Int = {
      val random = new Random()

      @tailrec
      def inner(): Int = {
        var number = -1
        game.whoseTurn match
          case PLAYER_1 => number = random.nextInt(6)
          case PLAYER_2 => number = random.nextInt(6) + START_2
        if game.checkInput(number) then number else inner()
      }
      inner()
    }

    def makeMove(hole: Int): Boolean = {
     splitStones(hole)
    }

    def findHole(): Int = {
      val tree = createTree(DEPTH, game)
      val result = minimax(tree, DEPTH)
      var bestHole = -1
      tree.children.foreach(child => if child.extraMove then bestHole = child.hole)
      if bestHole == -1 then tree.children.foreach(child =>
        if child.minimax == result then bestHole = child.hole)
      bestHole
    }

    def createTree(depth: Int, game: Game): Node = {
      val tree = new Node(new Game(game))
      val player = new Human(game)

      def deepen(node: Node, depth: Int): Unit = {
        @tailrec
        def createChildren(validMoves: List[Int], result: List[Node]): List[Node] = {
          validMoves match
            case h :: t =>
              val state = new Game(node.currentState)
              player.game = state
              val move = player.makeMove(h)
              //              node.extraMove = move
              val child = Node(state)
              child.hole = h
              child.extraMove = move
              state.whoseTurn = if state.whoseTurn == PLAYER_1 then PLAYER_2 else PLAYER_1
              createChildren(t, child :: result)
            case _ => result
        }

        node.children = createChildren(node.currentState.getValidMoves(), List())

        if depth > 0 then node.children.foreach(child => deepen(child, depth - 1))
      }

      deepen(tree, depth)
      tree
    }

    def printTree(node: Node): Unit = {
      print("(" + node.hole + ") ")
      if node.children != null then node.children.foreach(child => printTree(child))
    }

    def minimax(currentNode: Node, depth: Int): Int = {
      val whoseTurn = currentNode.currentState.whoseTurn

      def inner(currentNode: Node, depth: Int, maximizingPlayer: Boolean): Int = {
        if depth == 0 || currentNode.currentState.isEnd() then
          currentNode.currentState.whoseTurn = whoseTurn
          val difference = currentNode.currentState.getDifference()
          currentNode.minimax = difference
          difference
        else if maximizingPlayer then
          var maxEval = Integer.MIN_VALUE
          currentNode.children.foreach(child => {
            maxEval = Integer.max(maxEval, inner(child, depth - 1, false))
          })
          currentNode.minimax = maxEval
          maxEval
        else
          var minEval = Integer.MAX_VALUE
          currentNode.children.foreach(child => {
            minEval = Integer.min(minEval, inner(child, depth - 1, true))
          })
          currentNode.minimax = minEval
          minEval
      }
      inner(currentNode, depth, true)
    }
  }

  class Node(var currentState: Game) {
    var children: List[Node] = _
    var hole: Int = _
    var minimax: Int = _
    var extraMove: Boolean = _
  }
}
