package server
import gameboard.KalahaBoard
import gui.MainGUI
import player.Player
import player.HumanPlayer
import player.AIPlayer

// Server should only start when all settings are configured
class Server() {
  private var gui: MainGUI = _
  private var kalahaBoard: KalahaBoard = _
  private var player1: Player = _
  private var player2: Player = _
  // Which player currently moves
  private var firstPlayerMoves: Boolean = _
  // Max waiting time (in seconds) for hole choice
  private val MaxWaitTime: Int = 30

  def getGUI(): MainGUI = gui
  def getKalahaBoard(): KalahaBoard = kalahaBoard


  def startGUI(): Unit =
    gui = new MainGUI(this)

  def initializeGame(boardSize: Int, noStartingStones: Int, isHuman1: Boolean, isHuman2: Boolean): Unit =
    kalahaBoard = new KalahaBoard(boardSize, noStartingStones)
    if isHuman1 then player1 = new HumanPlayer(true)
    else player1 = new AIPlayer(true)
    if isHuman2 then player2 = new HumanPlayer(false)
    else player2 = new AIPlayer(false)
    firstPlayerMoves = true

  def playGame(): Unit =
    if firstPlayerMoves then {
      player1.getName() match {
        case "Human" => gui.changeLayoutToChooseHole(player1)
        case "AI" => {
          val chosenHole = player1.asInstanceOf[AIPlayer].chooseMove(kalahaBoard)
          var results = kalahaBoard.makeMoveOnBoard(chosenHole, firstPlayerMoves)
          firstPlayerMoves = results._2
          if kalahaBoard.getIsGameFinished(firstPlayerMoves) then printResults()
          else playGame()
        }
      }
    }

    else {
      player2.getName() match {
        case "Human" => gui.changeLayoutToChooseHole(player2)
        case "AI" => {
          val chosenHole = player2.asInstanceOf[AIPlayer].chooseMove(kalahaBoard)
          var results = kalahaBoard.makeMoveOnBoard(chosenHole, firstPlayerMoves)
          firstPlayerMoves = results._2
          if kalahaBoard.getIsGameFinished(firstPlayerMoves) then printResults()
          else playGame()
        }
      }
    }

  def usePlayerMove(chosenHole: Int): Unit =
    val results = kalahaBoard.makeMoveOnBoard(chosenHole, firstPlayerMoves)
    firstPlayerMoves = results._2
    if kalahaBoard.getIsGameFinished(firstPlayerMoves) then printResults()
    else playGame()

  private def printResults(): Unit =
    val (player1Results, player2Results): (Int, Int) = kalahaBoard.getResults()
    println("Player 1 final score: " + player1Results.toString)
    println("Player 2 final score: " + player2Results.toString)
    if player1Results == player2Results then println("Game ended in tie")
    else if player1Results > player2Results then println("Player 1 won")
    else println("Player 2 won")

  private def printError(): Unit =
    println("Game could not end because of the lack of activity from one of the players")
    sys.exit()



}
