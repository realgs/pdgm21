package Kalaha.KalahaAI

import Kalaha.Board.KalahaBoard.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

object Node {

  class Node(board: KalahaBoard, move: Int, var playerSymbol: String, var thisPlayer: String):
    val nextMoves: Array[Node] = Array(null, null, null, null, null, null)
    private val newBoard: KalahaBoard = board.copy()
    var bestMove: Int = -1
    private var nextPlayer: String = "A"

    private var leadsToResult: Int = -1
    private var seedsPitsA: Int = -1
    private var seedsPitsB: Int = -1

    if move != -1 then
      if thisPlayer == "A" then
        if newBoard.nextMoveA(move) then
          nextPlayer = "A"
        else
          nextPlayer = "B"
      else if newBoard.nextMoveB(move) then
        nextPlayer = "B"
      else
        nextPlayer = "A"

    def calculateNextMove(): Unit =
      for i <- 0 to 5 do
        if nextPlayer == "A" && newBoard.pitsA(i) != 0 then
          nextMoves(i) = new Node(newBoard, i, playerSymbol, nextPlayer)
        if nextPlayer == "B" && newBoard.pitsB(i) != 0 then
          nextMoves(i) = new Node(newBoard, i, playerSymbol, nextPlayer)

    def calculateTree(depth: Int, parallel: Boolean): Unit =
      if depth <= 0 then
        ()
      else
        calculateNextMove()

        if parallel then
          val futures =
            for (i <- 0 to 5 if nextMoves(i) != null) yield
              Future {
                nextMoves(i).calculateTree(depth - 1, false)
              }

          futures.map(Await.ready(_, Duration.Inf))

        else
          for i <- 0 to 5 do
            if nextMoves(i) != null then nextMoves(i).calculateTree(depth - 1, false)

    def calculateBestOutcome(parallel: Boolean): Unit =
      var noNextMove = true
      for i <- 0 to 5 do
        if nextMoves(i) != null then noNextMove = false

      if noNextMove then
        bestMove = -1
        leadsToResult = newBoard.result()
        seedsPitsA = newBoard.seedsA()
        seedsPitsB = newBoard.seedsB()
      else
        if parallel then
          val futures =
            for (i <- 0 to 5 if nextMoves(i) != null) yield
              Future {
                nextMoves(i).calculateBestOutcome(false)
              }

          futures.map(Await.ready(_, Duration.Inf))

        else
          for i <- 0 to 5 do
            if nextMoves(i) != null then nextMoves(i).calculateBestOutcome(false)

        if nextPlayer == "A" then
          bestMove = newBoard.firstAvailableMoveA()
        else
          bestMove = newBoard.firstAvailableMoveB()

        leadsToResult = nextMoves(bestMove).leadsToResult
        seedsPitsA = nextMoves(bestMove).seedsPitsA
        seedsPitsB = nextMoves(bestMove).seedsPitsB

        for i <- bestMove + 1 to 5 do
          if nextMoves(i) != null then
            if nextPlayer == "A" && leadsToResult < nextMoves(i).leadsToResult then
              bestMove = i
              leadsToResult = nextMoves(i).leadsToResult
              seedsPitsA = nextMoves(i).seedsPitsA
              seedsPitsB = nextMoves(i).seedsPitsB
            else
              ()
            if nextPlayer == "A" && leadsToResult == nextMoves(i).leadsToResult && seedsPitsA < nextMoves(i).seedsPitsA then
              bestMove = i
              leadsToResult = nextMoves(i).leadsToResult
              seedsPitsA = nextMoves(i).seedsPitsA
              seedsPitsB = nextMoves(i).seedsPitsB
            else
              ()

            if nextPlayer == "B" && leadsToResult > nextMoves(i).leadsToResult then
              bestMove = i
              leadsToResult = nextMoves(i).leadsToResult
              seedsPitsA = nextMoves(i).seedsPitsA
              seedsPitsB = nextMoves(i).seedsPitsB
            else
              ()
            if nextPlayer == "B" && leadsToResult == nextMoves(i).leadsToResult && seedsPitsB < nextMoves(i).seedsPitsB then
              bestMove = i
              leadsToResult = nextMoves(i).leadsToResult
              seedsPitsA = nextMoves(i).seedsPitsA
              seedsPitsB = nextMoves(i).seedsPitsB
            else
              ()
}
