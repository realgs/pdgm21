import scala.Array.*

class Board {

  private val defaultNumberOfStones = 6
  private val firstPlayer0 = 0
  private val firstPlayerBase = 6
  private val secondPlayer0 = 7
  private val secondPlayerBase = 13
  private var recentlyUsed = -1
  private var firtsPlayerActive = true

  val fields : Array[Int] = Array.fill(14)(defaultNumberOfStones)
  fields(firstPlayerBase) = 0
  fields(secondPlayerBase) = 0

  def emptyField(index : Int) : Boolean =
    fields(index) == 0

  def checkChoice(index : Int) : Boolean = {
    if(firtsPlayerActive){
      if(index >= 0 && index <= 5){
        if(emptyField(index)) false
        else true
      } else false
    } else {
      if (index >= 7 && index <= 12){
        if(emptyField(index)) false
        else true
      } else false
    }
  }

  def moveSeedsFrom(index : Int) : Unit = {
    var numberOfSeeds = fields(index)
    fields(index) = 0
    recentlyUsed = index
    while (numberOfSeeds > 0){
      recentlyUsed = (recentlyUsed + 1) % 14
      if (recentlyUsed == firstPlayerBase && !firtsPlayerActive){
        recentlyUsed = (recentlyUsed + 1) % 14
      }
      else if (recentlyUsed == secondPlayerBase && firtsPlayerActive){
        recentlyUsed = (recentlyUsed + 1) % 14
      }
      else {
        fields(recentlyUsed) += 1
        numberOfSeeds -= 1
      }

    }
    if (fields(recentlyUsed) == 1 && recentlyUsed != firstPlayerBase && recentlyUsed != secondPlayerBase){
      checkOppositeField(recentlyUsed)
    }
  }

  def getOppositeIndex(index : Int) : Int = {
    secondPlayerBase - index - 1
  }

  def checkOppositeField(index : Int) : Unit = {
    if (firtsPlayerActive) fields(firstPlayerBase) += (fields(getOppositeIndex(index)) + 1)
    else fields(secondPlayerBase) += (fields(getOppositeIndex(index)) + 1)

    fields(getOppositeIndex(index)) = 0
    fields(index) = 0
  }

  def nextPlayer() : Unit = {
    if (firtsPlayerActive && recentlyUsed != firstPlayerBase){
      firtsPlayerActive = false
    } else if(!firtsPlayerActive && recentlyUsed != secondPlayerBase){
      firtsPlayerActive = true
    }
  }

  def collectAllSeeds() : Unit = {
    if (firtsPlayerActive && fields.slice(0, 6).sum == 0){
      fields(secondPlayerBase) += fields.slice(7, 13).sum
    } else if(!firtsPlayerActive && fields.slice(7, 13). sum == 0){
      fields(firstPlayerBase) += fields.slice(0, 6).sum
    }
  }

  def isNextMovePossibe() : Boolean = {
    if(firtsPlayerActive && fields.slice(0, 6).sum == 0) false
    else if (!firtsPlayerActive && fields.slice(7, 13).sum == 0) false
    else true
  }

  def findActivePlayer() : Boolean = {
    firtsPlayerActive
  }

  def setActivePlayer(bool : Boolean) = {
    firtsPlayerActive = bool
  }

  def printBoard() : Unit = {
    println("")
    println("\t\t\t\t\t\tGracz 2")
    println("Pola:\t 5\t\t 4\t\t 3\t\t 2\t\t 1\t\t 0")
    println(s"\t\t[${fields(12)}]\t\t[${fields(11)}]\t\t[${fields(10)}]\t\t[${fields(9)}]\t\t[${fields(8)}]\t\t[${fields(7)}]")
    println(s"\t[${fields(13)}]\t\t\t\t\t\t\t\t\t\t\t\t [${fields(6)}]")
    println(s"\t\t[${fields(0)}]\t\t[${fields(1)}]\t\t[${fields(2)}]\t\t[${fields(3)}]\t\t[${fields(4)}]\t\t[${fields(5)}]")
    println("Pola:\t 0\t\t 1\t\t 2\t\t 3\t\t 4\t\t 5")
    println("\t\t\t\t\t\tGracz 1")
  }

  def calculate(player : Boolean) : Int = {
    var result = 0
    if (player){
      result = fields(firstPlayerBase) - fields(secondPlayerBase)
    } else {
      result = fields(secondPlayerBase) - fields(firstPlayerBase)
    }
    result
  }

  def cloneBoard() : Board = {
    val clone = new Board
    for (i <- fields.indices){
      clone.fields(i) = fields(i)
    }
    clone
  }

  def endResult() : Unit = {
    collectAllSeeds()
    val player1result = fields(firstPlayerBase)
    val player2result = fields(secondPlayerBase)

    println("Gracz 1 - wynik : " + player1result)
    println("Gracz 2 - wynik : " + player2result)


    if (player1result > player2result) println("Gracz 1 wygral")
    else if (player1result < player2result) println("Gracz 2 wygral")
    else println("Remis")
  }
}


