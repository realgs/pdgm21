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
      tree.children.foreach(child =>
        println("%d:min %d :hole".format(child.minimax, child.hole))
        if child.minimax == result then bestHole = child.hole)
      bestHole
    }

    def createTree(depth: Int, game: Game): Node = {
      val tree = new Node(new Game(game))
      tree.maximize = true
      val player = new Human(game)

      def deepen(parent: Node, depth: Int): Unit = {
        @tailrec
        def createChildren(validMoves: List[Int], result: List[Node]): List[Node] = {
          validMoves match
            case h :: t =>
              player.game =  new Game(parent.currentState)
              val move = player.makeMove(h)
              val child = Node(player.game)
              child.hole = h
              move match
                case true =>
                  child.maximize = parent.maximize
                  child.currentState.whoseTurn = parent.currentState.whoseTurn
                case false =>
                  child.maximize = !parent.maximize
                  child.currentState.whoseTurn = (parent.currentState.whoseTurn + 1) % 2
              createChildren(t, child :: result)
            case _ => result
        }
        parent.children = createChildren(parent.currentState.getValidMoves(), List())
        if depth > 0 then parent.children.foreach(child => deepen(child, depth - 1))
      }
      deepen(tree, depth)
      tree
    }

    def minimax(currentNode: Node, depth: Int): Int = {
      val whoseTurn = currentNode.currentState.whoseTurn

      def inner(currentNode: Node, depth: Int): Int = {
        if depth == 0 || currentNode.currentState.isEnd() then
          currentNode.currentState.whoseTurn = whoseTurn
          val difference = currentNode.currentState.getDifference()
          currentNode.minimax = difference
          difference
        else if currentNode.maximize then
          var maxEval = Integer.MIN_VALUE
          currentNode.children.foreach(child => {
            maxEval = Integer.max(maxEval, inner(child, depth - 1))
          })
          currentNode.minimax = maxEval
          maxEval
        else
          var minEval = Integer.MAX_VALUE
          currentNode.children.foreach(child => {
            minEval = Integer.min(minEval, inner(child, depth - 1))
          })
          currentNode.minimax = minEval
          minEval
      }
      inner(currentNode, depth)
    }
  }

  class Node(var currentState: Game) {
    var children: List[Node] = _
    var hole: Int = _
    var maximize: Boolean = _
    var minimax: Int = _
  }
}
