package Kalah
import java.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class Computer(s_name:String, index: Int) extends Opponent(s_name, index) {

  /*
  override
  def move(board: Array[Int]): Int = {
    val rd = new Random()
    var index=rd.nextInt(6) + 7 * this.index + 1
    while (index == 0 || index == 7){
      index=rd.nextInt(13) + 1
    }
    println(s_name+": "+index)
    index
  }
  */

  override
  def move(board: Board): Int = {
    val x = aiMove(board.getBoard)
    println(s_name+" = "+x)
    x
  }

  def aiMove(board: Array[Int]): Int = {
    var bestIndex = 1
    var bestPoints = 0
    for (boardIndex <- (7 * index + 1) to (7 * index + 6)) {
      //println("Sprawdzamy "+boardIndex)
      val f = Future {
        possiblePoints(boardIndex, board.clone())
      }

      if bestPoints < Await.result(f, Duration.Inf) then
        bestPoints = Await.result(f, Duration.Inf)
        bestIndex = boardIndex
        //println("Punkty "+bestPoints+" Dołek: "+bestIndex)
    }
    bestIndex
  }

  def possiblePoints(boardIndex: Int, board: Array[Int]):Int={
    var points = 0
    var boardID = boardIndex
    var seeds = board(boardID)
    board(boardID) = 0

    while (seeds != 0) {
      boardID = (boardID+1) % 14
      board(boardID)+=1
      if (this.index == 0 && boardID == 7) || (this.index == 1 && boardID == 0) then
        points+=1
      seeds-=1
    }

    if selfEmptyHouse(this.index, boardID, board) then
      board(7 * ((this.index * 1) % 2))+=board(boardID)
      points += board(boardID)
      board(boardID)=0
      board(7 * ((this.index + 1) % 2))+=board(14 - boardID)
      points += board(14 - boardID)
      board(14 - boardID)=0

    if this.index == 0 && boardID == 7 then
      points += board(this.aiMove(board))

    if this.index == 1 && boardID == 0 then
      points += board(this.aiMove(board))

    points
  }

  private def selfEmptyHouse(playerIndex:Int, boardIndex: Int, board: Array[Int]):Boolean={
    board(boardIndex) == 1 && boardIndex >= playerIndex * 7 + 1 && boardIndex <= playerIndex * 7 + 6
  } 

}
