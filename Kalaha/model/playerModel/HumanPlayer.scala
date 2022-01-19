package model.playerModel

import model.GameSpecification.{ARRAYOFHOUSESSTART, INDEXFIRSTSTORE, PLAYERHOUSESSTART}
import model.playerModel.Player
import model.decisionTreeModel.SingleDecision

class HumanPlayer(_arrOfHomes: Array[Int] = PLAYERHOUSESSTART.clone(), _score: Int = 0,private var isStartingGame: Boolean = true, newName: String = "P") extends Player(_arrOfHomes, _score):
  _name = newName
  private var board = ARRAYOFHOUSESSTART.clone()

  def getBoard(): Array[Int] = board

  def changeIsStarting(newValue: Boolean) = isStartingGame = newValue

  def getAvailableMoves(): IndexedSeq[Int] =
    val indexes = for index <- 0 until INDEXFIRSTSTORE if arrayOfHomes(index) != 0 yield index
    indexes

  def extractHomesFromBoard() =
    for index <- 0 until INDEXFIRSTSTORE do
      arrayOfHomes(index) = board(index)

  override def makeMove(moveIndex :Int = 0): (Int, Boolean, Boolean, SingleDecision) =
    val boardAfterMove = nextMove(score, moveIndex, true, board)
    board = boardAfterMove.arrayOfHouses

    extractHomesFromBoard()

    score = boardAfterMove.arrayOfHouses(INDEXFIRSTSTORE)

    (moveIndex, boardAfterMove.isEndingMove, boardAfterMove.isYourMove, boardAfterMove)

  def updateAfterEnemiesMove(moveIndex: Int): Unit =
    val newBoard = nextMove(score, moveIndex + 7, false, board)
    board = newBoard.arrayOfHouses

    extractHomesFromBoard()
    score = newBoard.arrayOfHouses(INDEXFIRSTSTORE)


  override def countEndingScore(): Int =
    var result = score
    for index <- 0 until INDEXFIRSTSTORE do
      result += arrayOfHomes(index)
    result
