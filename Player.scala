package players
import boards.Board.Board

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

  /*class HumanPlayer(board: Board, index: Int) extends Player(board, index){
    override def move(): Int={
      println("Your move [0-5]: ")
      val move = readLine()
      require(move.type ==Int)
    }
  }*/

}
