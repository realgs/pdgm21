package players
import boards.Board.Board

import java.nio.channels.InterruptedByTimeoutException
import scala.::
import scala.io.StdIn.readLine
import scala.util.Random

object Player {
  abstract class Player(board: Board, index: Int){

    def move(): Int
    
    
  }


  class SimulationPlayer(board: Board, index: Int) extends Player(board, index){

    override def move(): Int={
      val random = new Random()
      val index = random.between(0,6)
      index
    }
  }

  class HumanPlayer(board: Board, index: Int) extends Player(board, index){

    override def move(): Int={
      println(s"Your move [0-5]: ")
      var move = readLine()
      while(!(move=="0" || move=="1" || move =="2" || move=="3" || move=="4" || move =="5")){
        println("Incorrect input, try again")
        move = readLine()
      }
      move.toInt
    }
  }

  class AI(board: Board, index: Int) extends Player(board, index){

    override def move(): Int={
      BestMove( Tree())
    }

    def BestMove(lazylists: (List[Int], List[Int])): Int= {
      var best = -10000
      var bestID = 0

      board.printBoard()

      var i = board.Player1Row.size - 1
      while (i != -1) {
        if lazylists(0)(i)== -1 then i -=1
        if i != -1 then {
          //println(lazylists(0)(i) + "          " + lazylists(1)(i) + " " + i)
          if (best < (lazylists(0)(i) - lazylists(1)(i))) {
            best = lazylists(0)(i) - lazylists(1)(i)
            bestID = i
          }
          i -= 1
        }
      }
      bestID
    }


    def Tree(): (List[Int], List[Int])={

      var theoreticalHome: List[Int] = List()
      var theoreticalBestHomeforRival: List[Int] = List()

      var moves = board.Player1Row.size-1
      var moves2 = board.Player2Row.size-1
      var bestMoveRival = 0
      var secondPlayerID = 0

      if index==1 then secondPlayerID = 2
      else secondPlayerID =1
      var tempBoard: Board = new Board()
      var tempBoard2: Board = new Board()

      while(moves!= -1){

        tempBoard = new Board()
        tempBoard = board.copy()
        var x = false

        if (tempBoard.Player1Row(moves)==0 && index ==1)||(tempBoard.Player2Row(moves)==0 && index==2)  then{
          theoreticalHome = -1  :: theoreticalHome
          theoreticalBestHomeforRival = -1 :: theoreticalBestHomeforRival
          moves-=1
        }
        if moves== -1 then return (theoreticalHome,theoreticalBestHomeforRival)


        x = tempBoard.movingPieces(moves, index)


        if(x==true){
          if index==1 then tempBoard.Player1House += 1
          else tempBoard.Player2House += 1
        }


        if(index==1) {
          theoreticalHome = tempBoard.Player1House  :: theoreticalHome
        }
        else{
          theoreticalHome = tempBoard.Player2House :: theoreticalHome
        }

        moves2 = board.Player2Row.size-1
        bestMoveRival = 0

        while(moves2!= -1){
          tempBoard2 = new Board()
          tempBoard2 = tempBoard.copy()


         tempBoard2.movingPieces(moves, secondPlayerID)

          if(secondPlayerID == 2){
            if(tempBoard2.Player2House>bestMoveRival){
              bestMoveRival = tempBoard2.Player2House
            }
          }
          else{
            if(tempBoard2.Player1House>bestMoveRival){
              bestMoveRival = tempBoard2.Player1House
            }
          }
          moves2-=1
        }
        theoreticalBestHomeforRival = bestMoveRival :: theoreticalBestHomeforRival
        moves-=1
      }

      return (theoreticalHome,theoreticalBestHomeforRival)
    }
  }

}
