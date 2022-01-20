package kalaha

class Server {
  val game = new Board
  var player1: Player = null
  var player2: Player = null
  var currentPlayer: Player = null
  var gameStarted: Boolean = false

  def setPlayers(p1: Player, p2: Player): Boolean =
    player1 = p1
    player2 = p2
    true

  def startGame(): Unit =
    if player1 != null || player2 != null then init()

  def init(): Unit =
    gameStarted = true
    currentPlayer = player1

  def requestMove(hole: Int): Int =
    if !gameStarted then -3 // ERROR
    else if !checkForValidHole(hole - 1) then -1 // bad input
    else if game.isGameOver then -2 // game is finished
    else
      val response = game.makeMove(hole - 1)
      checkToSteal()
      if response then 1 // one more move
      else
        if currentPlayer == player1 then currentPlayer = player2 else currentPlayer = player1
        0 // nothing

  def getScore: (Int, Int) =
    game.getScore

  def isOver: Boolean =
    game.isGameOver

  def getBoard: String =
    game.getBoardString

  def getCurrentPlayer: Int =
    if currentPlayer == player1 then 1
    else 2

  def checkForValidHole(hole: Int): Boolean =
    if currentPlayer == player1 then
      if hole < 0 || hole > 5 then false // not in range
      else if game.board(hole) == 0 then false // empty
      else true
    else
      if hole < 7 || hole > 12 then false // not in range
      else if game.board(hole) == 0 then false // empty
      else true

  def getWinner: Int =
    if game.isGameOver then
      val score = game.getScore
      if score._1 > score._2 then 1
      else if score._1 < score._2 then 2
      else 0
    else -1

  def getCurrentScore: (Int, Int) =
    game.getScore

  def passTurn(): Unit =
    if currentPlayer == player1 then currentPlayer = player2 else currentPlayer = player1

  def checkToSteal(): Unit =
    if (currentPlayer == player1) then
      if (game.lastHole >= 0 && game.lastHole < 6) && game.board(game.lastHole) == 1 then
        game.stealFromHole(12 - game.lastHole, game.player1Base)
      else if (game.lastHole >= 7 && game.lastHole < 13) && game.board(game.lastHole) == 1 then
        game.stealFromHole(12 - game.lastHole, game.player2Base)

  def getBoardArray: Array[Int] =
    game.board

  def collectStonesToBase(): Unit =
    game.collectToBase()
}
