package kalahgame

class Board:

  private var board = Array(Array(6, 6, 6, 6, 6, 6, 0),Array(6, 6, 6, 6, 6, 6, 0))

  def pits: Array[Array[Int]] = board

  def pits_=(newPits: Array[Array[Int]]): Unit =
    board = newPits

  def printBoard(): Unit =
    println("\n\tPit index\t" + 5 + "\t" + 4 + "\t" + 3 + "\t" + 2 + "\t" + 1 + "\t" + 0)
    println("\t^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    println("\t[P2]\t\t" + board(0)(5) + "\t" + board(0)(4) + "\t" + board(0)(3) + "\t" + board(0)(2) + "\t" + board(0)(1) + "\t" + board(0)(0))
    println("\t\t\t" + board(0)(6) + "\t" + " " + "\t" + " " + "\t" + " " + "\t" + " " + "\t\t\t" + board(1)(6))
    println("\t\t\t\t" + board(1)(0) + "\t" + board(1)(1) + "\t" + board(1)(2) + "\t" + board(1)(3) + "\t" + board(1)(4) + "\t" + board(1)(5) + "\t\t\t[P1]")
    println("\t^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    println("\tPit index\t" + 0 + "\t" + 1 + "\t" + 2 + "\t" + 3 + "\t" + 4 + "\t" + 5 + "\n")

  def makePlay(selectedPit: Int, activePlayer: Int, flag: Boolean = true): Boolean =
    var bonusTurn = false
    var playerBoard = 1
    var enemyBoard = 1
    var helpPointer = 0
    var pitDropInto = 0
    var playerSide = false

    if (activePlayer == 1) then
      playerBoard = 1
      enemyBoard = 0
    else if (activePlayer == 2) then
      playerBoard = 0
      enemyBoard = 1

    var pointer = selectedPit
    var seedsSowNumber = board(playerBoard)(pointer)

    board(playerBoard)(selectedPit) = 0

    while
      seedsSowNumber > 0
    do
      pointer += 1
      helpPointer = pointer % 14

      if (helpPointer <= 6) then
        pitDropInto = helpPointer
        playerSide = true

      else if (helpPointer != 13) then
        pitDropInto = helpPointer % 7
        playerSide = false

      else
        pointer = 0
        pitDropInto = 0
        playerSide = true

      if (playerSide) then
        board(playerBoard)(pitDropInto) += 1

      else
        board(enemyBoard)(pitDropInto) += 1

      seedsSowNumber -= 1

    if (playerSide && pointer % 7 == 6) then
      bonusTurn = true

    else if (playerSide && pointer % 7 < 6) then
      val playerPit = pointer % 7
      val enemyPit = 5 - playerPit
      if ((board(playerBoard)(playerPit) == 1) && board(enemyBoard)(enemyPit) > 0) then
        board(playerBoard)(6) = board(playerBoard)(6) + board(enemyBoard)(enemyPit) + board(playerBoard)(playerPit)
        if(flag) then println("\t\t\t\t\t\t\t\t\tPlayer" + activePlayer + " captured " + board(enemyBoard)(enemyPit) + " of enemy's seeds for " + (board(enemyBoard)(enemyPit) + board(playerBoard)(playerPit)) + " points!")
        board(enemyBoard)(enemyPit) = 0
        board(playerBoard)(playerPit) = 0
    return bonusTurn

  def checkWin(): Boolean =
    var p1SeedsRemaining = 0
    var p2SeedsRemaining = 0
    for (i <- 0 until 6)
      p1SeedsRemaining = p1SeedsRemaining + board(1)(i)
      p2SeedsRemaining = p2SeedsRemaining + board(0)(i)

    if (p1SeedsRemaining == 0) then
      printBoard()
      for (i <- 0 until 6)
        board(0)(i) = 0

      board(0)(6) = board(0)(6) + p2SeedsRemaining
      return true

    else if (p2SeedsRemaining == 0) then
      printBoard()
      for (i <- 0 until 6)
        board(1)(i) = 0

      board(1)(6) = board(1)(6) + p1SeedsRemaining
      return true
    false

  def interruptedSumUp(): Unit =
    var p1SeedsRemaining = 0
    var p2SeedsRemaining = 0
    for (i <- 0 until 6)
      p1SeedsRemaining = p1SeedsRemaining + board(1)(i)
      p2SeedsRemaining = p2SeedsRemaining + board(0)(i)
    for (i <- 0 until 6)
      board(0)(i) = 0
      board(1)(i) = 0
    board(0)(6) = board(0)(6) + p2SeedsRemaining
    board(1)(6) = board(1)(6) + p1SeedsRemaining
    printBoard()


  def calcAdvantage(activePlayer: Int): Int =
    if(activePlayer == 1) then board(1)(6) - board(0)(6)
    else board(0)(6) - board(1)(6)

  def winner =
    if (board(1)(6) > board(0)(6)) then 1
    else if (board(1)(6) < board(0)(6)) then 2
    else
      println("\tIt's a draw!")
      -1

  def validSelection(selectedPit: Int, activePlayer: Int): Boolean =
    var playerBoard = 1
    if (activePlayer == 1) then playerBoard = 1
    else if (activePlayer == 2) then playerBoard = 0

    if (selectedPit < 0 || selectedPit > 5) then false
    else if (board(playerBoard)(selectedPit) < 1) then false
    else true

  def printPoints(): Unit =
    println("Player1: " + board(1)(6) + "\t" + " " + "\t" + " " + "\t" + " " + "\t" + " " + "\t\tPlayer2: " + board(0)(6))

  def copy(): Board =
    val newBoard = new Board
    newBoard.pits=board.map(_.clone())
    newBoard

end Board
