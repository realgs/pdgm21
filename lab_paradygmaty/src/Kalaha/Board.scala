package Kalaha

import scala.Array.*

class Board {

  private val defaultStonesNumber: Int = 4
  val firstBase: Int = 6
  val secondBase: Int = 13
  var firstPlayerMove: Boolean = true
  var currentPlayerLastStonePosition: Int = -1
  val gameBoard = new Array[Int](secondBase + 1)

  def prepareGame(): Unit = {
    for (i <- 0 until secondBase)
      gameBoard(i) = defaultStonesNumber
    gameBoard(firstBase) = 0
    gameBoard(secondBase) = 0
  }

  def cloneBoard(): Board = {
    val clone = new Board
    for (i <- gameBoard.indices) {
      clone.gameBoard(i) = gameBoard(i)
    }
    clone
  }

  def checkCorrectnessOfField(chosenField: Int): Boolean = {
    if (firstPlayerMove) {
      if (chosenField < 0 || chosenField > 5)
        false
      else if (gameBoard(chosenField) == 0)
        false
      else
        true
    } else {
      if (chosenField < 7 || chosenField > 12)
        false
      else if (gameBoard(chosenField) == 0)
        false
      else
        true
    }
  }

  def endOfGame(first: Boolean): Unit = {
    print("Koniec gry - zebranie pozostałych kamykow do bazy, kamyki zbiera: ")
    if first then
      println("Gracz 2")
      for (i <- 0 to 5)
        gameBoard(firstBase) = gameBoard(firstBase) + gameBoard(i)
        gameBoard(i) = 0
    else
      println("Gracz 1")
      for (i <- 7 to 12)
        gameBoard(secondBase) = gameBoard(secondBase) + gameBoard(i)
        gameBoard(i) = 0
  }

  def isNextMovePossibe(): Boolean = {
    if (firstPlayerMove && gameBoard.slice(0, 6).sum == 0) false
    else if (!firstPlayerMove && gameBoard.slice(7, 13).sum == 0) false
    else true
  }


  def makeMove(chosenField: Int): Unit = {
    val stonesFromField = gameBoard(chosenField)
    gameBoard(chosenField) = 0

    def moveHelper(index: Int, stones: Int): Unit = {
      if (stones > 1) {
        gameBoard(index) += 1
        val nextIndex = {
          if (index == secondBase)
            0
          else
            index + 1
        }
        moveHelper(nextIndex, stones - 1)
      } else {
        gameBoard(index) += 1
        currentPlayerLastStonePosition = index
      }
    }

    moveHelper(chosenField + 1, stonesFromField)
  }

  def takeStonesFromOpositeHoleIfItIsPossible(): Unit = {
    if (firstPlayerMove) {
      if (currentPlayerLastStonePosition >= 0 && currentPlayerLastStonePosition <= 5 && gameBoard(currentPlayerLastStonePosition) == 1) { // last stone was put into empty field
        val oppositeField = 12 - currentPlayerLastStonePosition
        gameBoard(firstBase) += gameBoard(oppositeField)
        gameBoard(oppositeField) = 0
      }
    } else {
      if (currentPlayerLastStonePosition >= 7 && currentPlayerLastStonePosition <= 12 && gameBoard(currentPlayerLastStonePosition) == 1) { // last stone was put into empty field
        val oppositeField = 12 - currentPlayerLastStonePosition
        gameBoard(secondBase) += gameBoard(oppositeField)
        gameBoard(oppositeField) = 0
      }
    }
  }

  def nextPlayer(): Unit = {
    if (firstPlayerMove) {
      if (currentPlayerLastStonePosition != firstBase) {
        firstPlayerMove = false
      }
      currentPlayerLastStonePosition = -1
    } else {
      if (currentPlayerLastStonePosition != secondBase) {
        firstPlayerMove = true
      }
      currentPlayerLastStonePosition = -1
    }
  }

  def countDifference(first: Boolean): Int =
    if first then gameBoard(firstBase) - gameBoard(secondBase)
    else gameBoard(secondBase) - gameBoard(firstBase)


  def printBoard(): Unit = {
    println("")
    println("\t\t\t\t\t\tGracz 2")
    println("Pola:\t 12\t\t 11\t\t 10\t\t 9\t\t 8\t\t 7")
    var j = 12
    while (j > 6) {
      print(s"\t\t[${gameBoard(j)}]")
      j = j - 1
    }
    println()
    println()
    println(s"\t[${gameBoard(13)}]\t\t\t\t\t\t\t\t\t\t\t\t [${gameBoard(6)}]")
    println()
    for (i <- 0 until 6) {
      print(s"\t\t[${gameBoard(i)}]")
    }
    println()
    println("Pola:\t 0\t\t 1\t\t 2\t\t 3\t\t 4\t\t 5")
    println("\t\t\t\t\t\tGracz 1")
  }

  private def calculateResult(): (Int, Int) = {
    for (i <- 0 to firstBase - 1) {
      gameBoard(firstBase) += gameBoard(i)
    }
    for (i <- firstBase + 1 to secondBase - 1) {
      gameBoard(secondBase) += gameBoard(i)
    }
    (gameBoard(firstBase), gameBoard(secondBase))
  }

  def printResults(): Unit = {
    val (p1Result, p2Result) = calculateResult()
    println("Gracz 1: " + p1Result + " kamyki w bazie")
    println("Gracz 2: " + p2Result + " kamyki w bazie")
    if (p1Result > p2Result)
      println("Gracz 1 wygrał!")
    else if (p2Result > p1Result)
      println("Gracz 2 wygrał!")
    else
      println("Remis!")
  }

}
