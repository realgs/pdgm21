class Board(private val startStonesNumber: Int = 6) {

  private val board: Array[Int] = createBoard()
  private var ifPlayer1Move = true
  private var ifOneMoreMove = false
  private var ifGameIsOver = false

  def makeMove(fieldNumber: Int): Boolean = {

    if(fieldNumber < 1 || fieldNumber > 6 || board(fieldNumber + (if(ifPlayer1Move) -1 else 6)) == 0) false
    else {

      var index = fieldNumber + (if(ifPlayer1Move) -1 else 6)
      var stones = board(index)

      board(index) = 0

      while (stones > 0) {

        index = (index + 1) % 14

        if (index != (if(ifPlayer1Move) 13 else 6)) {

          board(index) = board(index) + 1
          stones -= 1
        }
      }

      if(index == (if(ifPlayer1Move) 6 else 13)) ifOneMoreMove = true
      else if(board(index) == 1 && /*board(12 - index) != 0 &&*/ index >= (if(ifPlayer1Move) 0 else 7) && index <= (if(ifPlayer1Move) 5 else 12)) {

        board(if(ifPlayer1Move) 6 else 13) += board(12 - index) + 1
        board(12 - index) = 0
        board(index) = 0
        ifPlayer1Move = !ifPlayer1Move
        ifOneMoreMove = false

      } else {

        ifPlayer1Move = !ifPlayer1Move
        ifOneMoreMove = false
      }

      checkIfEndOfGame()

      if(ifGameIsOver) {

        for(i <- 0 to 5) {

          board(6) += board(i)
          board(13) += board(i + 7)
          board(i) = 0
          board(i + 7) = 0
        }
      }

      true
    }
  }

  private def createBoard(): Array[Int] = {

    val board = Array.fill(14)(startStonesNumber)
    board(6) = 0
    board(13) = 0

    board
  }

  def print(): Unit = {

    printf("%n    | %2d | %2d | %2d | %2d | %2d | %2d |%n", board(12), board(11), board(10), board(9), board(8), board(7))
    printf(" %2d |-----------------------------| %2d", board(13), board(6))
    printf("%n    | %2d | %2d | %2d | %2d | %2d | %2d |%n", board(0), board(1), board(2), board(3), board(4), board(5))
  }

  def ifCorrectFieldNumber(fieldNumber: Int): Boolean =
    fieldNumber >= 1 && fieldNumber <= 6 && board(fieldNumber + (if(ifPlayer1Move) -1 else + 6)) != 0

  def printResult(): Unit = {

    val player1Score = board(6)
    val player2Score = board(13)

    printf("%n# PLAYER 1. SCORE: %2d", player1Score)
    printf("%n# PLAYER 2. SCORE: %2d", player2Score)
    if(player1Score == player2Score) println("\n# DRAW!")
    else printf("%n%n# PLAYER %d. WON!%n", if(player1Score > player2Score) 1 else 2)
  }

  def checkIfEndOfGame(): Unit = {

    ifGameIsOver = true

    for (i <- if(ifPlayer1Move) 0 to 5 else 7 to 12)
      ifGameIsOver = ifGameIsOver && board(i) == 0
  }

  override def clone(): Board = {

    val clone = new Board(startStonesNumber)

    for(i <- 0 to 13)
      clone.board(i) = board(i)
    clone.ifPlayer1Move = ifPlayer1Move
    clone.ifOneMoreMove = ifOneMoreMove
    clone.ifGameIsOver = ifGameIsOver

    clone
  }

  def getScores:(Int, Int) = (board(6), board(13))

  def isPlayer1Move: Boolean = ifPlayer1Move

  def hasLastPlayerOneMoreMove: Boolean = ifOneMoreMove

  def isGameOver: Boolean = ifGameIsOver
}