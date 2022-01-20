import Kalaha.*
import Player.*
import scala.io.StdIn.*

object UserPlayer {
  class UserPlayer extends Player {
    override def chooseNextMove(board: Kalaha): Int =
      println("Choose one of your non-empty pits (1 - 6).")
      var pitNumber = readInt()
      while (!board.checkCorrectPitNumber(pitNumber))
        println("Input was incorrect. Choose one of your non-empty pits (1 - 6).")
        pitNumber = readInt()
      pitNumber
  }
}
