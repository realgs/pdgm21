object Kalaha {

  def makeChoice() : Int = {
      var choice = -1
      try {
        choice = scala.io.StdIn.readInt()
      } catch {
        case e : NumberFormatException => {
          print("\nInvalid input, try again: ")
          return makeChoice()
        }
      }
      if choice < 1 || choice > 4 then {
        print("\nInvalid input, try again: ")
        return makeChoice()
      }
      else choice
  }

  def printMenu() : Unit = {
    println("KALAHA GAME")
    println("1. Player vs Computer")
    println("2. Player vs Player")
    println("3. Computer vs Computer")
    println("4. Quit")
    print("Your choice: ")
    val choice = makeChoice()

    choice match {
      case 1 => {
        val player : Player = Player()
        val board : Board = Board(player, ComputerAI(player))
        board.startGame()
        board.gameEnd()
        print("\nType 'q' to go back to menu")
        scala.io.StdIn.readLine()
        board.clearScreen()
        printMenu()
      }
      case 2 => {
        val board : Board = Board()
        board.startGame()
        board.gameEnd()
        print("\nType 'q' to go back to menu")
        scala.io.StdIn.readLine()
        board.clearScreen()
        printMenu()
      }
      case 3 => {
        val computer1 : ComputerAI = ComputerAI()
        val computer2 : ComputerAI = ComputerAI(computer1)
        computer1.setEnemyPlayer(computer2)
        val board : Board = Board(computer1, computer2)
        board.startGame()
        board.gameEnd()
        print("\nType 'q' to go back to menu")
        scala.io.StdIn.readLine()
        board.clearScreen()
        printMenu()
      }
      case 4 => {

      }
    }
  }

  def main(args : Array[String]) : Unit = {
    printMenu()
  }
}
