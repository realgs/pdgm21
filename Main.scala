import scala.io.StdIn
import akka.actor.{ActorSystem, Props}

object Main extends App {


  printMenu()

  val optionNumber = chooseOption()

  if(optionNumber == 1) playerVsComputerGame()
  else if(optionNumber == 2) computerVsComputerGame()
  else System.exit(0)


  def printMenu(): Unit = {

    println("\n MENU")
    println("1. PLAYER VS COMPUTER")
    println("2. COMPUTER VS COMPUTER")
    println("3. EXIT\n")
  }

  def chooseOption(): Int = {

    var option: Int = 0
    var ifCorrectNumber: Boolean = false

    while(!ifCorrectNumber) {

      print("SELECT OPTION: ")

      try {

        option = StdIn.readInt()

        ifCorrectNumber = option >= 1 && option <= 3

        if(!ifCorrectNumber)
          println("\nWRONG NUMBER! TRY AGAIN.")

      } catch {

        case _: NumberFormatException => println("\nWRONG INPUT! TRY AGAIN.")
        case _: Exception => ()
      }
    }

    option
  }

  def playerVsComputerGame(): Unit = {

    println("\n# PLAYER VS COMPUTER")
    println("# YOU ARE PLAYER 1.")
    println("# COMPUTER IS PLAYER 2.")

    val system = ActorSystem()
    val board = new Board
    board.print()
    val player1 = system.actorOf(Props(new HumanPlayer), "player1")
    val player2 = system.actorOf(Props(new ComputerPlayer), "player2")
    val server = system.actorOf(Props(new Server(player1, player2, board)), "server")

    server ! Server.AskForMove
  }
  def computerVsComputerGame(): Unit = {

    println("\n# COMPUTER VS COMPUTER")

    val system = ActorSystem()
    val board = new Board
    board.print()
    val player1 = system.actorOf(Props(new ComputerPlayer), "player1")
    val player2 = system.actorOf(Props(new ComputerPlayer), "player2")
    val server = system.actorOf(Props(new Server(player1, player2, board)), "server")

    server ! Server.AskForMove
  }
}