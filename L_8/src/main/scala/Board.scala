class Board {
  val numberOfStones: Int = 6
  val sideSize: Int = 6
  val base0Index: Int = sideSize
  val base1Index: Int = sideSize * 2 + 1
  val board: Array[Int] = Array.fill(sideSize * 2 + 2)(numberOfStones)
  board(base0Index) = 0
  board(base1Index) = 0

  def move(hole: Int, player: Int): Int =
    var holeIndex: Int = hole
    var playerNr: Int = player

    if holeIndex < 0 || holeIndex > sideSize then return playerNr
    if playerNr == 1 then holeIndex = holeIndex + sideSize + 1
    if board(holeIndex) == 0 then return playerNr
    if (playerNr == 0 && holeIndex == base0Index) || (playerNr == 1 && holeIndex == base1Index) then return playerNr

    var currentNumberOfStones: Int = board(holeIndex)
    board(holeIndex) = 0

    while(currentNumberOfStones > 0) {
      holeIndex = holeIndex + 1
      if playerNr == 0 && holeIndex == base1Index then holeIndex = 0
      if playerNr == 1 && holeIndex == base0Index then holeIndex = base0Index + 1
      if holeIndex > base1Index then holeIndex = 0
      board(holeIndex) = board(holeIndex) + 1
      currentNumberOfStones = currentNumberOfStones - 1
    }

    if (playerNr == 0 && holeIndex == base0Index) || (playerNr == 1 && holeIndex == base1Index) then return playerNr
    if board(holeIndex) == 1 && ((holeIndex < base0Index && playerNr == 0) || (holeIndex > base0Index && player == 1)) then{
      var oppositeHoleIndex = sideSize * 2 - holeIndex
      if playerNr == 0 then {
        board(base0Index) = board(holeIndex) + board(oppositeHoleIndex) + board(base0Index)
      }
      else {
        board(base1Index) = board(holeIndex) + board(oppositeHoleIndex) + board(base1Index)
      }
      board(oppositeHoleIndex) = 0
    }

    if playerNr == 0 then playerNr = 1
    else playerNr = 0
    return playerNr

  def isThisTheEnd(player: Int): Boolean =
    if player == 0 then {
      for (i <- 0 to base0Index - 1) {
        if board {
          i
        } != 0 then return false
      }
      return true
    }
    else {
      for (i <- base0Index + 1 to base1Index - 1) {
        if board(i) != 0 then return false
      }
      return true
    }

  def result(): Int =
    if board(base0Index) > board(base1Index) then return 0
    if board(base0Index) < board(base1Index) then return 1
    else return -1

  def cloneBoard(): Board =
    var Board_clone: Board = new Board()
    for(i <- 0 to board.length - 1)
      Board_clone.board(i) = board(i)
    return Board_clone

  def printBoard(): Boolean =
    for (i <- 0 to board.length - 1)
      if i == 0 then print("P 1 side:  ")
      if i == 7 then print("P 2 side:  ")
      if i%7 == 6 then println("Base-" + board(i))
      else print(i%7 +  "-" + board(i) + "  ")
    println()
    return true

  def printScheme(): Unit =
    println("      Board scheme:")
    println("    5  4  3  2  1  0     <- Player 2 side")
    println("B2                    B1")
    println("    0  1  2  3  4  5     <- Player 1 side\n")
}
