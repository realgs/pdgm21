package kalahgame

class RandomComputer() extends Player:

  private var identity = 0

  def ID: Int = identity

  def ID_=(newId: Int): Unit =
    identity = newId

  def pickMove(board: Board): Int =
    print("\t\t\t\t\t\t\t\t\tPlayer" + identity + " choose pit: ")
    val r = new scala.util.Random
    var choice: Int = r.nextInt(6)
    while(!board.validSelection(choice, identity))
      choice = r.nextInt(6)
    println(choice)
    choice

end RandomComputer

