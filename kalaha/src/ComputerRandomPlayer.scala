import scala.util.Random
import Kalaha.*
import Player.*

object ComputerRandomPlayer {
  class ComputerRandomPlayer extends Player {
    override def chooseNextMove(board: Kalaha): Int =
      var pitNumber = Random.nextInt(6) + 1
      while (!board.checkCorrectPitNumber(pitNumber))
        pitNumber = Random.nextInt(6) + 1
      println("Player " + board.getWhoseTurn + " chose pit number " + pitNumber)
      pitNumber
  }
}
