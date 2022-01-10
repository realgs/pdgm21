package player
import scala.util.Random
import gameboard.KalahaBoard
import server.Server

class AIPlayer (isFirstPlayer: Boolean) extends Player (isFirstPlayer, "AI") {
  // Computer player can choose hole number that's higher than board size because
  // he knows all the indexing
  def chooseMove(kalahaBoard: KalahaBoard): Int =
    val (isNextMovePossible, chosenHole) = getHoleNextMoveIndex(kalahaBoard)
    if isNextMovePossible then return chosenHole
    else return chooseBestMove(kalahaBoard)

  // Pick hole after which the last stone lands in the base
  // Always pick the hole that's closest to the base
  private def getHoleNextMoveIndex(kalahaBoard: KalahaBoard): (Boolean, Int) =
    val (startHoleIndex, baseIndex, _, _): (Int, Int, Int, Int) = getPlayersIndices(kalahaBoard)
    for (i <- baseIndex - 1 to startHoleIndex by -1) {
      if kalahaBoard.getBoard()(i) + i == baseIndex then return (true, i - startHoleIndex)
    }

    return (false, -1)

  // Choose the least detrimental move (enemy points - our points = MIN)
  private def chooseBestMove(kalahaBoard: KalahaBoard): Int =
    val (startHoleIndex, baseIndex, enemyStartHoleIndex, enemyBaseIndex): (Int, Int, Int, Int) = getPlayersIndices(kalahaBoard)
    val differences: Array[Int] = Array.ofDim(baseIndex - startHoleIndex)

    for (i <- startHoleIndex to baseIndex - 1) {
      if kalahaBoard.getBoard()(i) == 0 then differences(i-startHoleIndex) = Int.MaxValue

      else {
        val copyBoard: KalahaBoard = kalahaBoard.copy()
        copyBoard.makeMoveOnBoard(i-startHoleIndex, isFirstPlayer)
        differences(i-startHoleIndex) = getBestScoreEnemy(copyBoard)
      }
    }

    // Randomly choose the best action
    val minDifference = differences.min
    var chosenHoles = List[Int]()
    for (i <- 0 to differences.length - 1) {
      if differences(i) == minDifference then chosenHoles = i :: chosenHoles
    }

    val random = new Random
    return chosenHoles(random.nextInt(chosenHoles.length))

  // Enemy chooses the best move and returns the best score that he can achieve
  // Note - we will give +1 point for the case where enemy manages to land his last stone in his base
  private def getBestScoreEnemy(kalahaBoard: KalahaBoard): Int =
    val (_, baseIndex, enemyStartHoleIndex, enemyBaseIndex): (Int, Int, Int, Int) = getPlayersIndices(kalahaBoard)
    val differences: Array[Int] = Array.ofDim(enemyBaseIndex - enemyStartHoleIndex)

    for (i <- enemyStartHoleIndex to enemyBaseIndex - 1) {
      if kalahaBoard.getBoard()(i) == 0 then differences(i-enemyStartHoleIndex) = Int.MinValue

      else {
        val copyBoard: KalahaBoard = kalahaBoard.copy()
        val (_, isSamePlayer): (Boolean, Boolean) = copyBoard.makeMoveOnBoard(i-enemyStartHoleIndex, !isFirstPlayer)

        differences(i-enemyStartHoleIndex) = copyBoard.getBoard()(enemyBaseIndex) - copyBoard.getBoard()(baseIndex)
        // Enemy can make the next move
        if isSamePlayer == !isFirstPlayer then differences(i-enemyStartHoleIndex) = differences(i-enemyStartHoleIndex) + 1
      }
    }

    return differences.max

  // Return your startHole, base and enemy's startHole and base index
  private def getPlayersIndices(kalahaBoard: KalahaBoard): (Int, Int, Int, Int) =
    val player1BaseIndex: Int = kalahaBoard.getPlayer1BaseIndex()
    val player2BaseIndex: Int = kalahaBoard.getPlayer2BaseIndex()
    if isFirstPlayer then return (0, player1BaseIndex, player1BaseIndex+1, player2BaseIndex)
    else return (player1BaseIndex+1, player2BaseIndex, 0, player1BaseIndex)
}
