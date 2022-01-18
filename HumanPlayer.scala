package player
import gameUtensils.KalahaBoard
import player.Player
import java.awt.event.ActionEvent
import scala.annotation.tailrec
import scala.io.StdIn.readInt

class HumanPlayer(override val playerID: Int, val currentBoardState: KalahaBoard) extends Player(playerID)
{
  @Override
  override def makeMove(): Int =
      println("PLayer " +playerID +" please make your move!")
      try {
        val input = readInt()
        if currentBoardState.isMoveCorrect(input) then
          println("PLayer " +playerID +" made move: " + input)
          input
        else
          println("You can't move stones from this pit!")
          makeMove()
      }
      catch {
        case e: Exception  => println("Index of pit must be an Integer!")
          makeMove()
      }


}
