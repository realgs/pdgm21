package player
import server.Server

class HumanPlayer (isFirstPlayer: Boolean) extends Player (isFirstPlayer) {
  private var chosenHole: Int = -1

  override def chooseMove(server: Server): Int =
    chosenHole = -1
    server.getGUI().changeLayoutToChooseHole(this)
    while (chosenHole == -1) {
      Thread.sleep(500)
    }
    /*var numberIsInteger: Boolean = false
    var chosenHole: Int = -1
    while (!numberIsInteger) {
      println("Choose hole number (hole to the left is hole number 1, hole next to it is 2 and so on)")
      val input: String = scala.io.StdIn.readLine()
      try {
        println()
        println(chosenHole)
        println()
        chosenHole = input.toInt
        numberIsInteger = true
      }
      catch {
        case _: NumberFormatException =>
      }
    }*/

    // Human player start indexing from 1 not 0

    return 1

  def setChosenHole(chosenHole: Int) = this.chosenHole = chosenHole
}
