package KalahaGame
import KalahaGame._


import scala.language.postfixOps

class Board {
  private val numberOfStones: Int = 6
  val board: Array[Int] = createBoard()
  private var if1PlayerMove = true
  private var ifOneMoreMove = false
  var ifGameIsOver = false

  def getScores:(Int, Int) = (board(6), board(13))

  def isPlayer1Move: Boolean = if1PlayerMove

  def hasLastPlayerOneMoreMove: Boolean = ifOneMoreMove

  def isGameOver: Boolean = ifGameIsOver

  def makeMove(fieldNumber: Int): Boolean = {
    if (fieldNumber < 1 || fieldNumber > 6 || board(fieldNumber +
      (if(if1PlayerMove) -1 else 6)) == 0) {
      println("\nInvalid input, try again:")
      false
    }
    else {
      var index = fieldNumber + (if(if1PlayerMove) -1 else 6)
      var stones = board(index)

      board(index) = 0

      while (stones > 0) {
        index = (index + 1) % 14
        if(index != (if(if1PlayerMove) 13 else 6)) {
          board(index) = board(index) + 1
          stones = stones - 1
        }
      }

      if (index == (if(if1PlayerMove) 6 else 13)) ifOneMoreMove = true
      else if (board(index) == 1 && index >= (if(if1PlayerMove) 0 else 7) && index <= (if(if1PlayerMove) 5 else 12)) {
        board(if(if1PlayerMove) 6 else 13) += board(12-index) + 1
        board(12 - index) = 0
        board(index) = 0
        if1PlayerMove = !if1PlayerMove
        ifOneMoreMove = false
      }
      else {
        if1PlayerMove = !if1PlayerMove
        ifOneMoreMove = false
      }

      ifEndOfGame()

      true
    }
  }

  private def createBoard(): Array[Int] = {
    val board = Array.fill(14)(numberOfStones)
    board(6) = 0
    board(13) = 0
    board
  }

  def ifEndOfGame(): Unit = {
    ifGameIsOver = true

    for (i <- if(if1PlayerMove) 0 to 5 else 7 to 12)
      ifGameIsOver = ifGameIsOver && board(i) == 0

    if(ifGameIsOver) {
      for(i <- 0 to 5) {
        board(6) += board(i)
        board(13) += board(i + 7)
        board(i) = 0
        board(i + 7) = 0
      }
      printScore()
    }
  }

  override def clone(): Board = {
    val clone = new Board
    for(i <- 0 to 13)
      clone.board(i) = board(i)
    clone.if1PlayerMove = if1PlayerMove
    clone.ifOneMoreMove = ifOneMoreMove
    clone.ifGameIsOver = ifGameIsOver

    clone
  }

  def ifCorrectFieldNumber(fieldNumber: Int): Boolean =
    fieldNumber >= 1 && fieldNumber <= 6 && board(fieldNumber + (if(if1PlayerMove) -1 else + 6)) != 0

  def printBoard(): Unit = {
      printf(f"      ${board(12)}%2d - ${board(11)}%2d - ${board(10)}%2d - ${board(9)}%2d - ${board(8)}%2d - ${board(7)}%2d %n" +
      f"(${board(13)}%2d )                              (${board(6)}%2d ) %n" +
      f"      ${board(0)}%2d - ${board(1)}%2d - ${board(2)}%2d - ${board(3)}%2d - ${board(4)}%2d - ${board(5)}%2d")
  }

  def printScore(): Unit = {
    val playerScore1 = board(6)
    val playerScore2 = board(13)

    println("\n-> 1. player score: " + playerScore1)
    println("-> 2. player score: " + playerScore2)
    if(playerScore1 == playerScore2)
      println("DRAW")
    else println("WINNER: " + (if(playerScore1 > playerScore2) "1. player: " + playerScore1 else "2. player: " + playerScore2))
  }

  def printScoreAfterTimeOut(): Unit = {
    ifGameIsOver = true
    for(i <- 0 to 5) {
      board(6) += board(i)
      board(13) += board(i + 7)
      board(i) = 0
      board(i + 7) = 0
    }
    printScore()
  }

}
