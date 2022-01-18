package player
import gameUtensils.KalahaBoard
import scala.util.Random

sealed trait DecisionTree
case object Empty extends DecisionTree
case class Node(board: KalahaBoard, diffrence: Int, firstMove: Int, move0: DecisionTree, move1: DecisionTree, move2: DecisionTree, move3: DecisionTree, move4: DecisionTree, move5: DecisionTree) extends DecisionTree

class ComputerPlayer(override val playerID: Int, val currentBoardState: KalahaBoard) extends Player(playerID)
{
  private val generateRandom = Random
  private val maxDepth = 3

//  @Override
//  override def makeMove(): Int =
//    Thread.sleep(50)
//    var index = if (playerID == 1) then 0 else 7
//    if (playerID == 1) then
//      index = generateRandom.between(0, 6)
//      while(currentBoardState.getNumberOfStonesInPit(index) == 0) {
//            index = generateRandom.between(0, 6)
//        }
//      index
//    else
//      index = generateRandom.between(7, 13)
//      while(currentBoardState.getNumberOfStonesInPit(index) == 0) {
//        index = generateRandom.between(7, 13)
//      }
//    index

  @Override
  override def makeMove(): Int =
    Thread.sleep(500)
    var move = 0
    if currentBoardState.onlyOneOption() != -1 then move = currentBoardState.onlyOneOption()
    else
      val tree = generateDecisionTree(currentBoardState, maxDepth)
      move = treeMax(tree)._2
    println("PLayer " +playerID +" made move: " + move)
    move

  def getBestMoveEnemy(boardState: KalahaBoard): (Int, Int) =
    val enemyID = if playerID == 1 then 2 else 1
    val offset = if playerID == 1 then 7 else 0
    var (bestResult, bestMove): (Int, Int) = (-48, 0)

    for(i<- 0 to 5)
      var copyOfBoard: KalahaBoard = boardState.copy()
      if copyOfBoard.isMoveCorrect(i + offset) == false then ()
      else
        copyOfBoard.performMoveOfOnePlayer(i)
        val enemyResult = copyOfBoard.resultForPlayer(enemyID)
        if enemyResult > bestResult then {bestResult = enemyResult; bestMove = i + offset} else ()

    (bestResult, bestMove)

  def generateDecisionTree(boardStart: KalahaBoard, depthStart: Int): DecisionTree =
    def decisionTreeGenerator(boardState: KalahaBoard, depth: Int, move: Int, firstMove: Int): DecisionTree =
    //println("TWORZNEIE DRZEWA")
      depth match
        case 0 => Empty
        case _ =>
          val nextBoard = boardState.copy()
          if nextBoard.isMoveCorrect(move) == false then Empty else
            val willThisMoveFinishGame = nextBoard.performMoveOfOnePlayer(move)
            if willThisMoveFinishGame then Node(nextBoard, nextBoard.resultForPlayer(playerID), firstMove, Empty, Empty, Empty, Empty, Empty, Empty)
            else
              nextBoard.performMoveOfOnePlayer(getBestMoveEnemy(nextBoard)._2)
            if nextBoard.getActivePlayer() == 1 then Node(nextBoard, nextBoard.resultForPlayer(playerID), firstMove, decisionTreeGenerator(nextBoard, depth - 1, 0, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 1, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 2, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 3, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 4, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 5, firstMove))
            else Node(nextBoard, nextBoard.resultForPlayer(playerID), firstMove, decisionTreeGenerator(nextBoard, depth - 1, 7, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 8, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 9, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 10, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 11, firstMove), decisionTreeGenerator(nextBoard, depth - 1, 12, firstMove))

    if playerID == 1 then Node(boardStart.copy(), -48, 1, decisionTreeGenerator(boardStart, depthStart, 0, 0), decisionTreeGenerator(boardStart, depthStart, 1, 1), decisionTreeGenerator(boardStart, depthStart, 2, 2), decisionTreeGenerator(boardStart, depthStart, 3, 3), decisionTreeGenerator(boardStart, depthStart, 4, 4), decisionTreeGenerator(boardStart, depthStart, 5, 5))
    else Node(boardStart.copy(), -48, 7, decisionTreeGenerator(boardStart, depthStart, 7, 7), decisionTreeGenerator(boardStart, depthStart, 8, 8), decisionTreeGenerator(boardStart, depthStart, 9, 9), decisionTreeGenerator(boardStart, depthStart, 10, 10), decisionTreeGenerator(boardStart, depthStart, 11, 11), decisionTreeGenerator(boardStart, depthStart, 12, 12))

  def treeMax(tree: DecisionTree): (Int, Int) =
    def treeMaxtHelper(nodesToVisit: DecisionTree): (Int, Int) =
      nodesToVisit match
        case Empty => (-49, -49)
        case Node(board, diffrence, firstMove, Empty, Empty, Empty, Empty, Empty, Empty) => (diffrence, firstMove)
        case Node(board, diffrence, firstMove, m0, m1, m2, m3, m4, m5) =>
          val (max0, max1, max2, max3, max4, max5) = (treeMaxtHelper(m0), treeMaxtHelper(m1), treeMaxtHelper(m2), treeMaxtHelper(m3), treeMaxtHelper(m4), treeMaxtHelper(m5))
          chooseBetterMove((diffrence, firstMove), bestDiffrence(max0, max1, max2, max3, max4, max5))
    treeMaxtHelper(tree)

  def bestDiffrence(max0: (Int, Int), max1: (Int, Int), max2: (Int, Int), max3: (Int, Int), max4: (Int, Int), max5: (Int, Int)): (Int, Int) =
    chooseBetterMove(max5, chooseBetterMove(chooseBetterMove(max0, max1), chooseBetterMove(max2, max3)))

  def chooseBetterMove(max0: (Int, Int), max1: (Int, Int)): (Int, Int) =
    if max0._1 > max1._1 then max0
    else max1
}

