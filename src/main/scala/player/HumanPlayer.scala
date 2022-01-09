package player
import gameboard.KalahaBoard

class HumanPlayer (firstPlayer: Boolean) extends Player (firstPlayer) {
  override def chooseMove(kalahaBoard: KalahaBoard): Int =
    var numberIsInteger: Boolean = false
    var chosenHole: Int = -1
    while (!numberIsInteger) {
      println("Choose hole number (hole to the left is hole number 1, hole next to it is 2 and so on)")
      val input: String = scala.io.StdIn.readLine()
      try {
        chosenHole = input.toInt
        numberIsInteger = true
      }
      catch {
        case _: NumberFormatException =>
      }
    }

    // Human player start indexing from 1 not 0
    return chosenHole-1
}
