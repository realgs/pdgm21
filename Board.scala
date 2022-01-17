package Board


class Board(private val BoardSize: Int , private val HowManyStonesonStart: Int) {
  private var board: Array[Int] = Array.fill(BoardSize * 2 + 2)(HowManyStonesonStart)
  private var BasePlayer1: Int = BoardSize
  private var BasePlayer2: Int = BoardSize * 2 + 1
  board(BasePlayer1) = 0
  board(BasePlayer2) = 0

  def getBoard(): Array[Int] = board
  def getBasePlayer1():Int = BasePlayer1
  def getBasePlayer2():Int = BasePlayer2
  def getBoardSize(): Int = BoardSize

  def Move(Hole: Int , Player: Int):  Int =
    var Idx: Int = Hole
    var player: Int = Player

    if Idx < 0 || Idx > BoardSize then return  player
    if player == 1 then Idx = Idx + BoardSize + 1
    if board(Idx) == 0 then return player

    var StonesInHole: Int = board(Idx)
    board(Idx) = 0
    while (StonesInHole != 0) {
        Idx = Idx + 1
        if player == 0 && Idx == BasePlayer2 then Idx = 0
        if player == 1 && Idx == BasePlayer1 then Idx = BasePlayer1 + 1
        if Idx > BasePlayer2 then Idx = 0
        board(Idx) = board(Idx) + 1
        StonesInHole = StonesInHole - 1
    }

    if (player == 0 && Idx == BasePlayer1) || (player == 1 && Idx == BasePlayer2) then return  player
    if board(Idx) == 1 && ((Idx < BasePlayer1 && Player == 0) || (Idx > BasePlayer1 && player == 1)) then{
      var OppositeIdx = BoardSize * 2 - Idx
      if player == 0 then {
        board(BasePlayer1) = board(Idx) + board(OppositeIdx) + board(BasePlayer1)
      }
      else{
        board(BasePlayer2) = board(Idx) + board(OppositeIdx) + board(BasePlayer2)
      }
      board(OppositeIdx) = 0
    }

    if player == 0 then player = 1
    else player = 0

    return player


  def IsFinished(Player: Int): Boolean =
    if Player == 0 then {
      for (i <- 0 to BasePlayer1 - 1) {
        if board(i) != 0 then return false
      }
      return true
    }
    else {
      for (i <- BasePlayer1 + 1 to BasePlayer2 - 1) {
        if board(i) != 0 then return false
      }
      return true
    }

  def Result(): Int =
    var Res1: Int = board(BasePlayer1)
    var Res2: Int = board(BasePlayer2)
    if Res1 > Res2 then return 0
    else return 1

  def CloneBoard(): Board =
    var BoardClone: Board = new Board(BoardSize,0 )
    for(i<- 0 to board.length-1)
      BoardClone.getBoard()(i) = board(i)
    return BoardClone

  def PrintBoard(): Boolean =
    for(i<- 0 to board.length-1)
      print(i + " Kulek: " + board(i) + " ")
    println()
    return true
}
