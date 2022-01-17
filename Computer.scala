package Client
import Game.Board

class Computer(name:  String, number : Int) extends Client(name, number) {

  override def whichHole(board: Board): Int =
    val startBase = if number == PLAYER1 then board.getBoard(BASE1).getPebbles
    else board.getBoard(BASE2).getPebbles
    Thread.sleep(1000)
    chooseBestMove(board.copy, number, startBase, -1)
    val choice = hole
    hole = -1
    max = -1
    println(choice)
    choice

  private val PLAYER1 = 1
  private val PLAYER2 = 2
  private val BASE1 = 6
  private val BASE2 = 13
  private val INDEX1 = 0
  private val INDEX2 = 7
  private val base = if number == PLAYER1 then BASE1
  else BASE2
  private val index = if number == PLAYER1 then INDEX1
  else INDEX2
  private var player = 0
  private var move = -1
  private var max = -1
  private var hole = -1
  private var copyBoard: Board = _

  def chooseBestMove(board: Board, numberOfPlayer: Int, startBase: Int, holeNumber: Int): Unit =
    for (i <- 0 to 5) {
      if (!board.emptyHole(i + index)) {
        copyBoard = board.copy
        player = copyBoard.movePebbles(i + 1, number)
        move = copyBoard.getBoard(base).getPebbles - startBase
        if move > max then {
          max = move
          if holeNumber == -1 then hole = i + 1
          else hole = holeNumber
        }
        if player == number then chooseBestMove(copyBoard, number, startBase, if holeNumber == -1 then i + 1 else holeNumber)
      }
    }
}