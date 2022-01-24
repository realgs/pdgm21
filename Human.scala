package kalahgame

class Human() extends Player:

  private var identity = 0
  private val poprawny = List("0", "1", "2", "3", "4", "5")

  def ID: Int = identity

  def ID_=(newId: Int): Unit =
    identity = newId

  def pickMove(board: Board): Int =
    print("\t\t\t\t\t\t\t\t\tPlayer" + identity + " choose pit: ")
    var selectedPit = scala.io.StdIn.readLine()
    while(!poprawny.contains(selectedPit))
      print("\t\t\t\t\t\t\t\t\tInvalid input - enter a digit within the range 0-5: ")
      selectedPit = scala.io.StdIn.readLine()
    selectedPit.toInt

end Human
