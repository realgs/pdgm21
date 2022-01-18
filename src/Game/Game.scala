package Game

import scala.annotation.tailrec

object Game {

  val MANCALA_1 = 6
  val MANCALA_2 = 13

  val START_1 = 0
  val START_2 = 7

  val PLAYER_1 = 0
  val PLAYER_2 = 1

  val HOLES_N = 14
  val EMPTY_HOLE = 0

  class Game(var board: Array[Int] = Array[Int](4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0), var whoseTurn: Int = 0) {

    def this(game: Game) = this(game.board.clone(), game.whoseTurn) // copying constructor

    def checkInput(input: Int): Boolean = {
      if (whoseTurn == PLAYER_1 && input >= START_1 && input < MANCALA_1 || whoseTurn == PLAYER_2 && input >= START_2 && input < MANCALA_2) && board(input) != EMPTY_HOLE then true
      else false
    }
    
    def getValidMoves(): List[Int] = {
      if isEnd() then List()
      else {
        @tailrec
        def inner(result: List[Int], current: Int, end: Int): List[Int] = {
          if current == end then result
          else if checkInput(current) then inner(current :: result, current + 1, end) else inner(result, current + 1, end)
        }
        if whoseTurn == PLAYER_1 then inner(List(), START_1, MANCALA_1)
        else inner(List(), START_2, MANCALA_2)
      }
    }
    
    def isOpponentMancala(hole: Int): Boolean = {
      if whoseTurn == PLAYER_1 && hole == MANCALA_2 || whoseTurn == PLAYER_2 && hole == MANCALA_1 then true
      else false
    }

    def inOwnHole(hole: Int): Boolean = {
      if whoseTurn == PLAYER_1 && hole >= START_1 && hole < MANCALA_1 || whoseTurn == PLAYER_2 && hole >= START_2 && hole < MANCALA_2 then true
      else false
    }

    def checkExtraMove(lastHole: Int): Boolean = {
      if whoseTurn == PLAYER_1 && lastHole == MANCALA_1 || whoseTurn == PLAYER_2 && lastHole == MANCALA_2 then true
      else false
    }

    def oppositeHole(hole: Int): Int = 12 - hole

    def canSteal(hole: Int): Boolean = {
      if inOwnHole(hole) && board(hole) == EMPTY_HOLE + 1 && board(oppositeHole(hole)) != EMPTY_HOLE then true
      else false
    }
    def stealStones(hole: Int): Unit = {
      if whoseTurn == PLAYER_1 then board(MANCALA_1) += board(oppositeHole(hole)) + 1
      else board(MANCALA_2) += board(oppositeHole(hole)) + 1
      board(hole) = EMPTY_HOLE
      board(oppositeHole(hole)) = EMPTY_HOLE
    }

    def sumOfStones(startIndex: Int, endIndex: Int): Int = {
      @tailrec
      def sum(currIndex: Int, result: Int): Int = {
        if currIndex == endIndex then result
        else sum(currIndex + 1, result + board(currIndex))
      }

      sum(startIndex, 0)
    }

    def getWinner(): Int = {
      val result1 = board(MANCALA_1) + sumOfStones(START_1, MANCALA_1)
      val result2 = board(MANCALA_2) + sumOfStones(START_2, MANCALA_2)
      if result1 > result2 then PLAYER_1
      else if result1 < result2 then PLAYER_2
      else -1
    }

    def getDifference(): Int = {
      if whoseTurn == PLAYER_1 then board(MANCALA_1) - board(MANCALA_2)
      else board(MANCALA_2) - board(MANCALA_1)
    }

    def isEnd(): Boolean = {
      if sumOfStones(START_1, MANCALA_1) == 0 || sumOfStones(START_2, MANCALA_2) == 0 then return true
      false
    }

    def printResult(winner: Int): String = {
      if winner == -1 then "-------------------DRAW---------------------"
      else s"-------------- PLAYER ${winner + 1} WON!!! --------------"
    }

    def printBoard(): String = {
      "\n---------------------------------------------" +
        "\n--------------- PLayer 2 area ---------------" +
        s"\n       12----11----10----9-----8-----7     " +
        "\n---------------------------------------------" +
        s"\n       ${board(12)}     ${board(11)}     ${board(10)}     ${board(9)}     ${board(8)}     ${board(7)}     " +
        s"\n   ${board(13)}                                      ${board(6)}" +
        s"\n       ${board(0)}     ${board(1)}     ${board(2)}     ${board(3)}     ${board(4)}     ${board(5)}         " +
        "\n---------------------------------------------" +
        s"\n       0-----1-----2-----3-----4-----5         " +
        "\n--------------- PLayer 1 area ---------------" +
        "\n---------------------------------------------\n"
    }
    
    def printPlayerOptions(): String = {
      "\n 1) Player" +
      "\n 2) Computer random" +
      "\n 3) Computer AI, minimax depth: 1" +
      "\n 4) Computer AI, minimax depth: 5" +
      "\n Your choice: "
    }
  }
}
