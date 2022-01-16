import scala.annotation.tailrec

object Main {

  def printMenu(): Unit =
    println()
    println("##### Kalaha Game #####")
    println("Choose playing mode: ")
    println("1 <= RandomPlayer vs RandomPlayer")
    println("2 <= RandomPlayer vs HumanPlayer")
    println("3 <= RandomPlayer vs AIPlayer")
    println("4 <= HumanPlayer vs HumanPlayer")
    println("5 <= HumanPlayer vs RandomPlayer")
    println("6 <= HumanPlayer vs AIPlayer")
    println("7 <= AIPlayer vs AIPlayer")
    println("8 <= AIPlayer vs RandomPlayer")
    println("9 <= AIPlayer vs HumanPlayer")
    println()



  @tailrec
  def chooseMode(): Server =

    var mode = -1
    try
      mode = scala.io.StdIn.readInt()
    catch{
      case e: NumberFormatException => println("This is not a number")
    }

      mode match
        case 1 => Server(RandomPlayer(true), RandomPlayer(false))
        case 2 => Server(RandomPlayer(true), HumanPlayer(false))
        case 3 => Server(RandomPlayer(true), AIPlayer(false))
        case 4 => Server(HumanPlayer(true), HumanPlayer(false))
        case 5 => Server(HumanPlayer(true), RandomPlayer(false))
        case 6 => Server(HumanPlayer(true), AIPlayer(false))
        case 7 => Server(AIPlayer(true), AIPlayer(false))
        case 8 => Server(AIPlayer(true), RandomPlayer(false))
        case 9 => Server(AIPlayer(true), HumanPlayer(false))
        case _ =>
          println("Choose number between 1 and 9")
          chooseMode()





  def main(args: Array[String]): Unit = {

    printMenu()
    val server = chooseMode()
    server.play()

  }
}
