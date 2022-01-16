class HumanPlayer(first: Boolean) extends Player(first) {

  override def makeMove(board: Board): Int =
    var house = -1
    
    try
      house = scala.io.StdIn.readInt()
    catch{
      case e: NumberFormatException =>
        println("This is not a number.")
        if first then print("Player1's move: ")
        else print("Player2's move: ")
        house = makeMove(board)
    }
    house
    

    

}
