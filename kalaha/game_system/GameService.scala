package game_system

import board.Board
import players.Player

import scala.concurrent.*
import scala.concurrent.duration.*
import ExecutionContext.Implicits.global
import scala.io.StdIn.readInt

class GameService {

  private var board: Board = _
  private var player1: Player = _
  private var player2: Player = _
  private var currentPlayer: Player = _
  private var currentPlayerId: Int = _
  private var winner: Player = _
  private var isGameOver: Boolean = false
  private var hasNextMove: Boolean = false
  private var userChoice: Int = 0

  def initialize(board: Board, player1: Player, player2: Player): Unit =
    this.board = board
    this.player1 = player1
    this.player2 = player2
    currentPlayer = player1
    currentPlayerId = player1.id

    while !isGameOver do
      board.printBoard()
      checkMove()
      update()
      checkResult()
      switchPlayer()
      //println("curr_id: " + currentPlayerId)
    captureRestStones() //problem when time out
    board.printBoard()
    chooseWinner()


  def switchPlayer(): Unit =
    if !hasNextMove then
      if currentPlayerId == player1.id then
        currentPlayer = player2
        currentPlayerId = player2.id
      else
        currentPlayer = player1
        currentPlayerId = player1.id
    else
      hasNextMove = false

  def checkMove(): Unit =
    try
      val playerMoveFut = Future {
        userChoice = currentPlayer.makeMove()

        while !validateMove(userChoice) do
          userChoice = currentPlayer.makeMove()
      }
      Await.result(playerMoveFut, Duration(GameServer.TIME_PER_PLAYER_MOVE, "s"))
    catch
      case _: Throwable =>
        isGameOver = true
        println("Time is out.")
        if currentPlayerId == player1.id then //punish player for not playing
          board.player1Base = -10
        else
          board.player2Base = -10


  def update(): Unit =
    if isGameOver then return

    var houseVal = 0
    if currentPlayerId == player1.id then
      houseVal = board.player1Houses(userChoice - 1)
    else
      houseVal = board.player2Houses(userChoice - 1)
    if houseVal == 0 then return //empty house means there's no need to move

    val boardApprox = approximateBoard()

    //distribute all stones except 1
    boardApprox(userChoice - 1) = 0
    var iter = userChoice
    while houseVal > 1 do
      boardApprox(iter % boardApprox.length) += 1
      iter += 1
      houseVal -= 1

    //what to do with the last one stone:
    val lastPos = iter % boardApprox.length
    if lastPos == boardApprox.length / 2 then //base - another turn for the same player
      boardApprox(lastPos) += 1
      hasNextMove = true
    else if (boardApprox(lastPos) == 0) && (lastPos < GameServer.HOUSE_NR) then //empty house on the player site - player gets opponent's stones
      boardApprox(boardApprox.length / 2) += boardApprox(boardApprox.length - lastPos - 1)
      boardApprox(boardApprox.length - lastPos - 1) = 0
      boardApprox(lastPos) += 1
    else  //normal turn
      boardApprox(lastPos) += 1

    updateBoard(boardApprox)


  def checkResult(): Unit =
    if !isGameOver then

      isGameOver = true
      if currentPlayerId == player1.id then
        board.player1Houses.foreach(elem => {
          if (elem != 0) isGameOver = false
        })
      else
        board.player2Houses.foreach(elem => {
          if (elem != 0) isGameOver = false
        })
    else ()


  def chooseWinner(): Unit =
    if board.player1Base > board.player2Base then
      winner = player1
    else if board.player2Base > board.player1Base then
      winner = player2
    else
      winner = null

    println("Game Over")
    if winner == null then
      println("Tie")
    else
      println("Player " + winner.id + " won!")


  def captureRestStones(): Unit =

    if board.player1Base < 0 || board.player2Base < 0 then return //player lost by time-out; no need to count all stones

    var stones = 0
    board.player1Houses.foreach(x => stones += x)
    board.player1Houses.indices.foreach(i => board.player1Houses(i) = 0)
    board.player1Base += stones
    stones = 0
    board.player2Houses.foreach(x => stones += x)
    board.player2Houses.indices.foreach(i => board.player2Houses(i) = 0)
    board.player2Base += stones


  private def validateMove(userMove: Int): Boolean =
    //out of range move
    if(userMove < 1 || userMove > GameServer.HOUSE_NR)
      println("Incorrect.")
      return false

    //empty house move
    var houseVal = -1
    if currentPlayerId == player1.id then
      houseVal = board.player1Houses(userChoice - 1)
    else
      houseVal = board.player2Houses(userChoice - 1)
    if(houseVal == 0)
      println("Incorrect. Empty house.")
      return false

    true


  //convert board to 1D array depending on current player
  private def approximateBoard(): Array[Int] =
    if currentPlayerId == player1.id then
      board.player1Houses ++ Array(board.player1Base) ++ board.player2Houses
    else
      board.player2Houses ++ Array(board.player2Base) ++ board.player1Houses


  //apply changes to the board
  private def updateBoard(boardApprox: Array[Int]): Unit =
    if currentPlayerId == player1.id then
      board.player1Houses.indices.foreach(i => board.player1Houses(i) = boardApprox(i))
      board.player1Base = boardApprox(boardApprox.length / 2)
      board.player2Houses.indices.foreach(i => board.player2Houses(i) = boardApprox(boardApprox.length / 2 + 1 + i))
    else
      board.player2Houses.indices.foreach(i => board.player2Houses(i) = boardApprox(i))
      board.player2Base = boardApprox(boardApprox.length / 2)
      board.player1Houses.indices.foreach(i => board.player1Houses(i) = boardApprox(boardApprox.length / 2 +  1 + i))
}
