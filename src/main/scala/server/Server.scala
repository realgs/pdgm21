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
  // Game parameters
  private var boardSize: Int = 0
  private var noStartingStones: Int = 0
  private var isHuman1: Boolean = false
  private var isHuman2: Boolean = false

  def getGUI(): MainGUI = gui
  def getKalahaBoard(): KalahaBoard = kalahaBoard
  def setBoardSize(boardSize: Int): Unit = this.boardSize = boardSize
  def setNoStartingStones(noStartingStones: Int): Unit = this.noStartingStones = noStartingStones
  def setIsHuman1(isHuman1: Boolean): Unit = this.isHuman1 = isHuman1
  def setIsHuman2(isHuman2: Boolean): Unit = this.isHuman2 = isHuman2

  def startGUI(): Unit =
    gui = new MainGUI(this)

  def initializeGame(): Unit =
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
          if kalahaBoard.getIsGameFinished(firstPlayerMoves) then gui.changeLayoutToShowResults()
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
          if kalahaBoard.getIsGameFinished(firstPlayerMoves) then gui.changeLayoutToShowResults()
          else playGame()
        }
      }
    }

  def usePlayerMove(chosenHole: Int): Unit =
    val results = kalahaBoard.makeMoveOnBoard(chosenHole, firstPlayerMoves)
    firstPlayerMoves = results._2
    if kalahaBoard.getIsGameFinished(firstPlayerMoves) then gui.changeLayoutToShowResults()
    else playGame()
}
