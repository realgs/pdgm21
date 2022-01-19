package players


import game.Board

import scala.util.Random

class Computer(playerNum: Int, val b: Board) extends Player(playerNum){

  def board = b
  def chooseMove(): Int = {
    Thread.sleep(3000)
    simulateMoves(b)
  }

  def simulateMoves(board: Board): Int = {
    var myHouses: Array[Int] = Array()

    if playerNum == 1 then {
      myHouses = board.player1houses
    }
    else {
      myHouses = board.player2houses
    }
    var movesToChoose = Array[Int]()

    var bestOption = -1
    var bestPoints = -100
    for (i <- 0 to myHouses.size - 2) {
      if (myHouses(i) != 0) {
        val points = getPointsFromMove(i + 1, board.copyBoard())
        if ((points._1 - points._2) >= bestPoints) then {
          if((points._1 - points._2) == bestPoints){
            movesToChoose = i +: movesToChoose
          }else{
            movesToChoose = Array(i)
            bestOption = i
            bestPoints = (points._1 - points._2)
          }
        }
      }
    }
    val myChoice = new Random().between(0, movesToChoose.size)
    movesToChoose(myChoice) + 1
  }

  def getPointsFromMove(house: Int, board: Board): (Int, Int) = {
    var initialPoints = 0
    var myHouses: Array[Int] = Array()
    var opponentsHouses: Array[Int] = Array()
    var opponentsNum = 0

    if playerNum == 1 then {
      opponentsNum = 2
      myHouses = board.player1houses
      opponentsHouses = board.player2houses
      initialPoints = board.player1houses(board.baseIndex)
    }
    else {
      opponentsNum = 1
      myHouses = board.player2houses
      opponentsHouses = board.player1houses
      initialPoints = board.player2houses(board.baseIndex)
    }

      val cond = board.move(playerNum, house)
      if (cond) then {
        var bestOption = -1
        var bestPoints = -1
        var bestResult = (0, 0)
        for (i <- 0 to myHouses.size - 2) {
          if (myHouses(i) != 0) {
            val result = getPointsFromMove(i + 1, board.copyBoard())
            if ((result._1 - result._2) > bestPoints) then {
              bestResult = result
              bestPoints = (result._1 - result._2)
            }
          }
        }
        (bestResult._1 + 1, bestResult._2)
      }
      else {
        var pointsAfterMove = myHouses(board.baseIndex)

        var bestOption = -1
        var bestOpponentsPoints = -1
        for (i <- 0 to opponentsHouses.size - 2) {
          if (opponentsHouses(i) != 0) {
            val points = getOpponentsPointsFromMove(opponentsNum, i + 1, board.copyBoard())
            if (points > bestOpponentsPoints) then {
              bestOption = i
              bestOpponentsPoints = points
            }
          }
        }
        (pointsAfterMove - initialPoints, bestOpponentsPoints)
      }
    }

    def getOpponentsPointsFromMove(player: Int, house: Int, board: Board): Int = {
      var initialPoints = 0
      var opponentsHouses: Array[Int] = Array()

      if player == 1 then {
        opponentsHouses = board.player1houses
        initialPoints = board.player1houses(board.baseIndex)
      }
      else {
        opponentsHouses = board.player2houses
        initialPoints = board.player2houses(board.baseIndex)
      }
      if (board.move(player, house)) then {
        var bestOption = -1
        var bestOpponentsPoints = -1
        for (i <- 0 to opponentsHouses.size - 2) {
          if (opponentsHouses(i) != 0) {
            val points = getOpponentsPointsFromMove(player, i + 1, board.copyBoard())
            if (points > bestOpponentsPoints) then {
              bestOption = i
              bestOpponentsPoints = points
            }
          }
        }
        bestOpponentsPoints + 1
      }
      else {
        var pointsAfterMove = opponentsHouses(board.baseIndex)
        pointsAfterMove - initialPoints
      }
    }
}