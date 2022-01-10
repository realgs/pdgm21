package com.example.GamePackage

object Game {

  def getFirstPlayer(): Int = { 1 }

  def createBoardArray(numberOfStones: Int): Array[Int] = {
    def createBoardArrayInner(currentIndex:Int, numberOfStones:Int): List[Int] =
      if (currentIndex > 13) Nil
      else if ((currentIndex + 1)%7 == 0) 0 :: createBoardArrayInner(currentIndex + 1, numberOfStones)
      else numberOfStones :: createBoardArrayInner(currentIndex + 1, numberOfStones)
    createBoardArrayInner(0, numberOfStones).toArray
  }
}

class Game(private var board: Array[Int], private var activePlayer: Int) {
  private val PLAYER_1 = 1
  private val PLAYER_2 = 2
  private val INDEX1_PLAYER1 = 0
  private val INDEX1_PLAYER2 = 7
  private val HOLE_PLAYER1 = 6
  private val HOLE_PLAYER2 = 13
  private val HOLES_NUMBER = 14

  // game processing helpers

  def cloneBoard(): Game = {
    new Game(board.clone(), activePlayer)
  }

  def getBoardHole(index: Int): Int = {
    board(index)
  }


  def getActivePlayerNumber: Int =  {
    activePlayer
  }

  def changeActivePlayer(): Unit = {
    if (activePlayer == PLAYER_1) activePlayer = PLAYER_2
    else activePlayer = PLAYER_1
  }


  def checkIfEnd(): Boolean = {
    if ((board.slice(INDEX1_PLAYER1, HOLE_PLAYER1).sum == 0 && activePlayer == PLAYER_1) || (board.slice(INDEX1_PLAYER2, HOLE_PLAYER2).sum == 0 && activePlayer == PLAYER_2)) true
    else false
  }


  //process game

  def processPlayerMove(chosenField: Int): Boolean = {
    if (checkChosenField(chosenField)) {
      val stonesNumber = board(chosenField)
      board(chosenField) = 0

      def processPlayerMoveInner(index: Int, stonesToReplace: Int): Unit =
        if (stonesToReplace > 0) {

          if (activePlayer == PLAYER_1) {
            if (index != HOLE_PLAYER2) {
              board(index) = board(index) + 1
              processPlayerMoveInner((index + 1) % HOLES_NUMBER, stonesToReplace - 1)
            } else processPlayerMoveInner(INDEX1_PLAYER1, stonesToReplace)

          } else {
            if (index != HOLE_PLAYER1) {
              board(index) = board(index) + 1
              processPlayerMoveInner((index + 1) % HOLES_NUMBER, stonesToReplace - 1)
            } else processPlayerMoveInner(INDEX1_PLAYER2, stonesToReplace)
          }

        } else allStonesMoved(index)
      processPlayerMoveInner(chosenField + 1, stonesNumber)
      true
    }else false
  }

  def checkChosenField(field: Int): Boolean = {
    if (board(field) == 0) false
    else if (activePlayer == PLAYER_1) {
      if (field < INDEX1_PLAYER1 || field > HOLE_PLAYER1 - 1) false else true
    } else {
      if (field < INDEX1_PLAYER2 || field > HOLE_PLAYER2 - 1) false else true
    }
  }

  def allStonesMoved(index: Int): Unit = {
    val lastStoneIndex = if (index == 0) HOLE_PLAYER2 else index - 1
    lastStone(lastStoneIndex)
  }

  private def lastStone(indexOfLastMovedStone: Int): Unit = {
    if ((activePlayer == PLAYER_1 && board(indexOfLastMovedStone) - 1 == 0 && indexOfLastMovedStone >= INDEX1_PLAYER1 && indexOfLastMovedStone < HOLE_PLAYER1) || (activePlayer == PLAYER_2 && board(indexOfLastMovedStone) - 1 == 0 && indexOfLastMovedStone >= INDEX1_PLAYER2 && indexOfLastMovedStone < HOLE_PLAYER2))
        stealStones(indexOfLastMovedStone)
  }

  def stealStones(indexOfHole: Int): Unit = {
    val indexToStealFrom = 12 - indexOfHole
    board(indexOfHole) += board(indexToStealFrom)
    board(indexToStealFrom) = 0
  }


  // get and present results

  def countPlayerResultsDifference(): Int = {
    if (activePlayer == PLAYER_1) board(HOLE_PLAYER1) - board(HOLE_PLAYER2)
    else board(HOLE_PLAYER2) - board(HOLE_PLAYER1)
  }

  private def countPlayersResults(): (Int, Int) = {
    (board.slice(INDEX1_PLAYER1, HOLE_PLAYER1 + 1).sum, board.slice(INDEX1_PLAYER2, HOLE_PLAYER2 + 1).sum)
  }

  def printGameResult(): String = {
    val gameResult = countPlayersResults()
    if (gameResult._1 > gameResult._2)
      s"Player 1 won the game - results \n ${gameResult._1} : ${gameResult._2}"
    else if (gameResult._2 > gameResult._1)
      s"Player 2 won the game - results \n ${gameResult._2} : ${gameResult._1}"
    else
      s"There is no winner \n ${gameResult._1} : ${gameResult._2}"
  }

  private def printWhoPlay(): String = {
    if (activePlayer == PLAYER_1)
      "          Player 1 play          \n"
    else
      "          Player 2 play          \n"
  }

  def printGameBoard: String = {
    printWhoPlay() +
      s"\n    ${board(12)} | ${board(11)} | ${board(10)} | ${board(9)} | ${board(8)} | ${board(7)}\n" +
      s"    (${board(13)})                                    (${board(6)})\n" +
      s"    ${board(0)} | ${board(1)} | ${board(2)} | ${board(3)} | ${board(4)} | ${board(5)}\n"
  }
}
