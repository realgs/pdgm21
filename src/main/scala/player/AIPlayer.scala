package player
import board.KalahaBoard

class AIPlayer (firstPlayer: Boolean) extends Player (firstPlayer) {
  // Computer player can choose hole number that's higher than board size because
  // he knows all the indexing
  override def chooseMove(kalahaBoard: KalahaBoard): Int =
    val (isNextMovePossible, chosenHole) = getHoleNextMoveIndex(kalahaBoard)
    if isNextMovePossible then return chosenHole
    else return chooseBestMove(kalahaBoard)

  // Check if picking stones from any hole will guarantee any points.
  // If yes than choose this hole. If there is more that one hole choose the first hole
  private def getHoleNextMoveIndex(kalahaBoard: KalahaBoard): (Boolean, Int) =
    val (startHoleIndex, baseIndex, _, _): (Int, Int, Int, Int) = getPlayersIndices(kalahaBoard)
    for (i <- startHoleIndex to baseIndex - 1) {
      if kalahaBoard.getBoard()(i) + i == baseIndex then return (true, i - startHoleIndex)
    }

    return (false, -1)

  // Choose the least detrimental move (the move that makes enemy earn the least points possible)
  private def chooseBestMove(kalahaBoard: KalahaBoard): Int =
    val (startHoleIndex, baseIndex, enemyStartHoleIndex, enemyBaseIndex): (Int, Int, Int, Int) = getPlayersIndices(kalahaBoard)
    val highestDifference: Array[Int] = Array.ofDim(baseIndex - startHoleIndex - 1)

    // Select all actions
    for (i <- startHoleIndex to baseIndex - 1) {
      if kalahaBoard.getBoard()(i) == 0 then highestDifference(i-startHoleIndex) = Int.MaxValue
      else {
        kalahaBoard.makeMoveOnBoard(i, isFirstPlayer)
      }
    }

    return 1

  // Enemy chooses the best move and returns the best score that he can achieve
  private def enemyChoosesBestMove(kalahaBoard: KalahaBoard): Int =
    1

  // Return your startHole, base and enemy's startHole and base index
  private def getPlayersIndices(kalahaBoard: KalahaBoard): (Int, Int, Int, Int) =
    val (player1BaseIndex, player2BaseIndex): (Int, Int) = kalahaBoard.getBaseIndices()
    if isFirstPlayer then return (0, player1BaseIndex, player1BaseIndex+1, player2BaseIndex)
    else return (player1BaseIndex+1, player2BaseIndex, 0, player1BaseIndex)
}
