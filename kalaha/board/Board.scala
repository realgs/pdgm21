package board

import game_system.GameParameters.HOUSE_NR
import players.PlayerID
import players.PlayerID.PlayerID

class Board(private val houseNr: Int, private val stonesPerHouseNr: Int) {

  private var _player1Base = 0
  private var _player2Base = 0
  private var _player1Houses = new Array[Int](houseNr)
  private var _player2Houses = new Array[Int](houseNr)
  private var hasNextMove = false

  for(i <- 0 until houseNr)
    _player2Houses(i) = stonesPerHouseNr
    _player1Houses(i) = stonesPerHouseNr

  def printBoard(): Unit =
    println()
    print("Player2\t\t # \t")
    _player2Houses.reverse.foreach(house => print(house + "\t"))
    println(" #\t\tPlayer1")
    println("\t" + _player2Base + "\t\t # \t\t\t\t\t\t\t # \t\t" + _player1Base)
    print("\t\t\t # \t")
    _player1Houses.foreach(house => print(house + "\t"))
    println(" #")

  override def clone(): Board =
    val newBoard = new Board(houseNr, stonesPerHouseNr)
    newBoard.player1Base = player1Base
    newBoard.player1Houses = player1Houses.clone()
    newBoard.player2Base = player2Base
    newBoard.player2Houses = player2Houses.clone()
    newBoard

  def player1Base: Int =
    _player1Base

  def player2Base: Int =
    _player2Base

  def player1Base_=(base: Int): Unit =
    _player1Base = base

  def player2Base_=(base: Int): Unit =
    _player2Base = base

  def player1Houses: Array[Int] =
    _player1Houses
  
  def player1Houses_=(houses: Array[Int]): Unit =
    _player1Houses = houses

  def player2Houses: Array[Int] =
    _player2Houses
  
  def player2Houses_=(houses: Array[Int]): Unit =
    _player2Houses = houses

  def getScore(playerID: PlayerID): Int =
    if playerID == PlayerID.first then
      player1Base
    else
      player2Base

  def executeMove(move: Int, playerID: PlayerID): Unit =
    hasNextMove = false

    var houseVal = 0
    if playerID == PlayerID.first then
      houseVal = player1Houses(move - 1)
    else
      houseVal = player2Houses(move - 1)
    if houseVal == 0 then return //empty house means there's no need to move

    val boardApprox = approximateBoard(playerID)

    //distribute all stones except 1
    boardApprox(move - 1) = 0
    var iter = move
    while houseVal > 1 do
      boardApprox(iter % boardApprox.length) += 1
      iter += 1
      houseVal -= 1

    //what to do with the last one stone:
    val lastPos = iter % boardApprox.length
    if lastPos == boardApprox.length / 2 then //base - another turn for the same player
      boardApprox(lastPos) += 1
      hasNextMove = true
    else if (boardApprox(lastPos) == 0) && (lastPos < HOUSE_NR) then //empty house on the player site - player gets opponent's stones
      boardApprox(boardApprox.length / 2) += boardApprox(boardApprox.length - lastPos - 1)
      boardApprox(boardApprox.length - lastPos - 1) = 0
      boardApprox(lastPos) += 1
    else  //normal turn
      boardApprox(lastPos) += 1

    updateBoard(boardApprox, playerID)

  def checkIfTheSamePlayerHasNextMove(): Boolean =
    hasNextMove

  def checkIfEmptyHouse(pos: Int, playerID: PlayerID): Boolean =
    var houseVal = -1
    if playerID == PlayerID.first then
      houseVal = player1Houses(pos)
    else
      houseVal = player2Houses(pos)

    houseVal == 0

  def checkIfMoreMovesPossible(playerID: PlayerID): Boolean =
    var hasMoreMoves = false
    if playerID == PlayerID.first then
      player1Houses.foreach(elem => {
        if (elem != 0) hasMoreMoves = true
      })
    else
      player2Houses.foreach(elem => {
        if (elem != 0) hasMoreMoves = true
      })
    hasMoreMoves

  //convert board to 1D array depending on current player
  private def approximateBoard(playerID: PlayerID): Array[Int] =
    if playerID == PlayerID.first then
      player1Houses ++ Array(player1Base) ++ player2Houses
    else
      player2Houses ++ Array(player2Base) ++ player1Houses

  //apply changes to the board
  private def updateBoard(boardApprox: Array[Int], playerID: PlayerID): Unit =
    if playerID == PlayerID.first then
      player1Houses.indices.foreach(i => player1Houses(i) = boardApprox(i))
      player1Base = boardApprox(boardApprox.length / 2)
      player2Houses.indices.foreach(i => player2Houses(i) = boardApprox(boardApprox.length / 2 + 1 + i))
    else
      player2Houses.indices.foreach(i => player2Houses(i) = boardApprox(i))
      player2Base = boardApprox(boardApprox.length / 2)
      player1Houses.indices.foreach(i => player1Houses(i) = boardApprox(boardApprox.length / 2 +  1 + i))
}
