package kalaha
import kalaha.Board.createBoard

object Board {
  def createBoard(numberOfStones: Int): Array[Int] = {
    val board = Array.fill(14)(6)
    board(6) = 0
    board(13) = 0
    board
  }
}

class Board {
  var board: Array[Int] = createBoard(6)
  val player1Base = 6
  val player2Base = 13
  val NUMBER_OF_HOLES = 14
  var lastHole: Int = -1

  def getScore: (Int, Int) =
    (board(player1Base), board(player2Base))

  def getBoardString: String =
    f"    | ${board(12)}%2d | ${board(11)}%2d | ${board(10)}%2d | ${board(9)}%2d | ${board(8)}%2d | ${board(7)}%2d | %n" +
    f"( ${board(13)}%2d )                           ( ${board(6)}%2d ) %n" +
    f"    | ${board(0)}%2d | ${board(1)}%2d | ${board(2)}%2d | ${board(3)}%2d | ${board(4)}%2d | ${board(5)}%2d |"

  def isGameOver: Boolean =
    board.slice(0, player1Base).sum == 0 || board.slice(player1Base + 1, player2Base).sum == 0

  def makeMove(hole: Int): Boolean =
      var index = (hole + 1) % NUMBER_OF_HOLES
      while (board(hole) != 0) {
        board(hole) -= 1
        board(index) += 1
        index = (index + 1) % NUMBER_OF_HOLES
      }
      lastHole = if (index - 1 == -1) 13 else index - 1
      lastHole == player1Base && (hole >= 0 && hole < 6) || lastHole == 13 && (hole > 6 && hole < 13)// one more move from player

  def stealFromHole(holeSrc: Int, holeDest: Int): Unit =
    board(holeDest) += board(holeSrc)
    board(holeSrc) = 0

  def collectToBase(): Unit =
    board.slice(0, player1Base).foreach(stones => {
      board(player1Base) += stones
    })
    board.slice(player1Base + 1, player2Base).foreach(stones => board(player2Base) += stones)
}
