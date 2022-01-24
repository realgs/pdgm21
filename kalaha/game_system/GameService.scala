package game_system

import board.Board
import players.{AIPlayer, Player}
import players.PlayerID.PlayerID

import scala.concurrent.*
import scala.concurrent.duration.*
import ExecutionContext.Implicits.global
import scala.io.StdIn.readInt

class GameService {

  private var board: Board = _
  private var player1: Player = _
  private var player2: Player = _
  private var currentPlayer: Player = _
  private var winner: Player = _
  private var isGameOver: Boolean = false
  private var hasNextMove: Boolean = false
  private var userChoice: Int = 0

  def initialize(board: Board, player1: Player, player2: Player): Unit =
    this.board = board
    this.player1 = player1
    this.player2 = player2
    currentPlayer = player1

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
      if currentPlayer.id == player1.id then
        currentPlayer = player2
      else
        currentPlayer = player1
    else
      hasNextMove = false

  def checkMove(): Unit =
    try
      val playerMoveFut = Future {
        currentPlayer match
          case player: AIPlayer => userChoice = player.makeMove(board)
          case _ => userChoice = currentPlayer.makeMove()

        while !validateMove(userChoice) do
          currentPlayer match
            case player: AIPlayer => userChoice = player.makeMove(board)
            case _ => userChoice = currentPlayer.makeMove()
      }
      Await.result(playerMoveFut, Duration(GameParameters.TIME_PER_PLAYER_MOVE, "s"))
    catch
      case _: Throwable =>
        isGameOver = true
        println("Time is out.")
        if currentPlayer.id == player1.id then //punish player for not playing
          board.player1Base = -10
        else
          board.player2Base = -10


  def update(): Unit =
    if !isGameOver then
      board.executeMove(userChoice, currentPlayer.id)
      hasNextMove = board.checkIfTheSamePlayerHasNextMove()


  def checkResult(): Unit =
    if !isGameOver then
      isGameOver = !board.checkIfMoreMovesPossible(currentPlayer.id)


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
    if(userMove < 1 || userMove > GameParameters.HOUSE_NR)
      println("Incorrect.")
      return false

    //empty house move
    if(board.checkIfEmptyHouse(userMove - 1, currentPlayer.id))
      println("Incorrect. Empty house.")
      return false

    true
}
