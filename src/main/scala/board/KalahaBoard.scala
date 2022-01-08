package board

class KalahaBoard(private val boardSize: Int, private val noStartingStones: Int)  {
  // Board consists of 2 rows for each player and 2 bases for each player
  private var board: Array[Int] = Array.fill(boardSize * 2 + 2)(noStartingStones)
  private val player1BaseIndex = boardSize
  private val player2BaseIndex = boardSize * 2 + 1
  // Bases should be initialized with 0
  board(player1BaseIndex) = 0
  board(player2BaseIndex) = 0

  // The board will be printed appriopriately for every player
  def printBoard(firstPlayerMoves: Boolean): Unit =
    if firstPlayerMoves then {
      println("Current game board")
      println("Enemy row")
      for (i <- player2BaseIndex - 1 to player1BaseIndex + 1 by -1)
        print(board(i) + " ")
      println("\nEnemy score: " + board(player2BaseIndex).toString + "\n")
      println("Your row")
      for (i <- 0 to player1BaseIndex-1)
        print(board(i) + " ")
      println("\nYour score: " + board(player1BaseIndex).toString + "\n")
    }

    else {
      println("Current game board")
      println("Enemy row")
      for (i <- player1BaseIndex - 1 to 0 by -1)
        print(board(i) + " ")
      println("\nEnemy score: " + board(player1BaseIndex).toString + "\n")
      println("Your row")
      for (i <- player1BaseIndex + 1 to player2BaseIndex - 1)
        print(board(i) + " ")
      println("\nYour score: " + board(player2BaseIndex).toString + "\n")
    }


  // Returns if the move was made correctly and what player
  // Also returns which player should make the next move
  def makeMoveOnBoard(chosenHole: Int, firstPlayerMoves: Boolean): (Boolean, Boolean) =
    var holeIndex: Int = chosenHole - 1
    if holeIndex < 0 || holeIndex >= boardSize then return (false, firstPlayerMoves)
    // Choose correct hole based on which player starts the move
    if !firstPlayerMoves then holeIndex = holeIndex + boardSize + 1
    // Current hole is empty
    if board(holeIndex) == 0 then return (false, firstPlayerMoves)

    var noStones: Int = board(holeIndex)
    board(holeIndex) = 0
    while (noStones != 0) {
      holeIndex = holeIndex + 1
      if holeIndex > player2BaseIndex then holeIndex = 0
      board(holeIndex) = board(holeIndex) + 1
      noStones = noStones - 1
    }

    // Perform specific actions mentioned in Kahala guidelines if necessary
    // Last stone is in the player's base
    if (firstPlayerMoves && holeIndex == player1BaseIndex) ||
      (!firstPlayerMoves && holeIndex == player2BaseIndex) then return (true, firstPlayerMoves)

    // Last stone was put in the empty player's hole (note that hole now contains one stone)
    if board(holeIndex) == 1 &&
      ((holeIndex < player1BaseIndex && firstPlayerMoves) || (holeIndex > player1BaseIndex && !firstPlayerMoves)) then {
      var oppositeHoleIndex = (boardSize * 2) - holeIndex
      board(holeIndex) = board(holeIndex) + board(oppositeHoleIndex)
      board(oppositeHoleIndex) = 0
    }

    // Last stone isn't in the player's base
    return (true, !firstPlayerMoves)

  def getIsGameFinished(firstPlayerMoves: Boolean): Boolean =
    if firstPlayerMoves then {
      for (i <- 0 to player1BaseIndex-1)
        if board(i) != 0 then return false

      return true
    }

    else {
      for (i <- player2BaseIndex - 1 to player1BaseIndex + 1 by -1)
        if board(i) != 0 then return false

      return true
    }

  def getResults(): (Int, Int) =
    var player1Result: Int = 0
    var player2Result: Int = 0
    for (i <- 0 to player1BaseIndex)
      player1Result = player1Result + board(i)
    for (i <- player1BaseIndex + 1 to player2BaseIndex)
      player2Result = player2Result + board(i)

    return (player1Result, player2Result)
}
