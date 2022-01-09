package server
import board.KalahaBoard
import player.Player
import player.HumanPlayer
import player.AIPlayer

// Server should only start when all settings are configured
class Server() {
  private var board: KalahaBoard = _
  private var player1: Player = _
  private var player2: Player = _
  // Which player currently moves
  private var firstPlayerMoves: Boolean = _
  // Max waiting time for hole choice
  private val MaxWaitTime: Int = 30


  def initializeGame(boardSize: Int, noStartingStones: Int, isHuman1: Boolean, isHuman2: Boolean): Unit =
    board = new KalahaBoard(boardSize, noStartingStones)

    if isHuman1 then player1 = new HumanPlayer(true)
    else player1 = new AIPlayer(true)

    if isHuman2 then player2 = new HumanPlayer(false)
    else player2 = new AIPlayer(false)

    firstPlayerMoves = true

  def playGame(): Unit =
    while (!board.getIsGameFinished(firstPlayerMoves)) {
      makeMovePlayer()
    }
    this.printResults()

  private def printResults(): Unit =
    val (player1Results, player2Results): (Int, Int) = board.getResults()
    if player1Results == player2Results then println("Game ended in tie")
    else if player1Results > player2Results then println("Player 1 won")
    else println("Player 2 won")

  def makeMovePlayer(): Unit =
    board.printBoard(firstPlayerMoves)

    var chosenHole: Int = 0
    if firstPlayerMoves then chosenHole = player1.chooseMove(board)
    else chosenHole = player2.chooseMove(board)

    board.makeMoveOnBoard(chosenHole, firstPlayerMoves)


}
