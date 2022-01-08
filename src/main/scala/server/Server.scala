package server
import board.KalahaBoard
import player.Player

// Server should only start when all settings are configured
class Server() {
  private var board: KalahaBoard = _
  private var player1: Player = _
  private var player2: Player = _
  // Which player currently moves
  private var firstPlayerMoves: Boolean = _
  // Max waiting time for hole choice
  private val MaxWaitTime: Int = 30


  def initializeGame(boardSize: Int, noStartingStones: Int, player1: Player, player2: Player): Unit =
    board = new KalahaBoard(boardSize, noStartingStones)
    this.player1 = player1
    this.player2 = player2
    firstPlayerMoves = true

  def playGame(): Unit =
    while (!board.getIsGameFinished(firstPlayerMoves)) {
      makeMovePlayer()
    }
    this.printResults()

  private def printResults(): Unit =
    val playerResults: (Int, Int) = board.getResults()
    if playerResults(0) == playerResults(1) then println("Game ended in tie")
    else if playerResults(0) > playerResults(1) then println("Player 1 won")
    else println("Player 2 won")

  def makeMovePlayer(): Unit =
    board.printBoard(firstPlayerMoves)
    var chosenHole: Int = 0
    if firstPlayerMoves then chosenHole = player1.chooseMove(board)
    else chosenHole = player2.chooseMove(board)
    board.makeMoveOnBoard(chosenHole, firstPlayerMoves)


}
