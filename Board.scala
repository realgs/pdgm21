class Board {
  val numberOfStones: Int = 6
  val board: Array[Int] = makeBoard()
  var isMoveFirstPlayer = true
  var isCanMoreMove = false
  var isGameEnd = false

  def makeBoard(): Array[Int] = {
    val board = Array.fill(14)(numberOfStones)
    board(6) = 0
    board(13) = 0
    board
  }

  def getSourse: (Int, Int) = (board(6), board(13))

  def playerIndex(number: Int): Int =
    if (isMoveFirstPlayer) return number
    else number + 7

  def MakeMove(holdN: Int) = {

    if (isMoveFirstPlayer) then if (holdN < 1 || holdN > 6 || board(holdN -1 ) == 0) throw new Exception(s"invalid hold: $holdN")
    if (!isMoveFirstPlayer) then if (holdN < 6 || holdN > 12 || board(holdN ) == 0) throw new Exception(s"invalid hold: $holdN")
3
    //  var index = holdN + playerIndex(-1)
    var index = if (holdN <= 6) then holdN - 1 else holdN
    var stone = board(index)

    board(index) = 0

    while (stone > 0) {
      index = (index + 1) % 14
      if (index != playerIndex(-13) * (-1)) {
        board(index) = board(index) + 1
        stone = stone - 1
      }
    }

    isGameEnd = true
    // check if the game is over
    for (i <- playerIndex(0) to playerIndex(5)) {
      isGameEnd = isGameEnd && board(i) == 0
    }
    if (isGameEnd) {
      EndGame()
    }

    // changing the rook
    if (index == playerIndex(6)) isCanMoreMove = true
    else {
      isMoveFirstPlayer = !isMoveFirstPlayer
      isCanMoreMove = false
    }

  }

  def EndGame(): Unit = {
    for (i <- 0 to 5) {
      board(6) += board(i)
      board(13) += board(i + 7)
      board(i) = 0
      board(i + 7) = 0
    }
    printScore()
  }

  def printScore(): Unit = {
    val player1 = board(6)
    val player2 = board(13)

    println(s"Score player 1: $player1")
    println(s"Score player 2: $player2")
    if (player1 > player2) return println("Winner player 1")
    else if (player1 < player2) return println("Winner player 2")
    else println("Draw")
  }

  override def clone(): Board = {
    val clone = new Board
    for (i <- 0 to 13)
      clone.board(i) = board(i)
    clone.isMoveFirstPlayer = isMoveFirstPlayer
    clone.isCanMoreMove = isCanMoreMove
    clone.isGameEnd = isGameEnd

    clone
  }

  def printBoard(): Unit = {
    printf(f"       12  -   11  -   10   -   9   -   8   -   7   %n" +
      f"      [${board(12)}%2d ] - [${board(11)}%2d ] - [${board(10)}%2d ] - [${board(9)}%2d ] - [${board(8)}%2d ] - [${board(7)}%2d ] %n" +
      f"(${board(13)}%2d )                                               (${board(6)}%2d ) %n" +
      f"      [${board(0)}%2d ] - [${board(1)}%2d ] - [${board(2)}%2d ] - [${board(3)}%2d ] - [${board(4)}%2d ] - [${board(5)}%2d ] %n" +
      f"       1  -   2   -   3   -   4   -   5   -   6   %n")
  }
}
