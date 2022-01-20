/* class responsible for choosing game mode, getting logic engine ready, granting the right to move to clients,
* */
import java.awt.Robot
import java.awt.event.KeyEvent
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Await, Future, TimeoutException}
import scala.io.StdIn.readLine

class Server {
  private var game: Game = _

  val P1 = "P1";
  val P2 = "P2";

  def start(): Unit = { //game starts here
    var input = ""
    println("WELCOME TO KALAHA!")
    println("Menu: \n " +
      "0 - Print instructions \n " +
      "1 - player vs player mode\n " +
      "2 - player vs computer mode \n " +
      "3 - computer vs computer mode\n")

    input = readLine
    input match {
      case "0" => showInstructions()
      case "1" => playerPlayerGame()
      case "2" => playerComputerGame()
      case "3" => computerComputerGame()
      case _ => {
        println("Wrong value! Try again")
        start()
      }
    }
  }

  def playTurn(client1: Client, client2: Client): Unit = { //function called in each turn
    val robot = new Robot()
    game.printCurrentBoard()
    implicit val execution = ExecutionContext.global

    var nextTurn = ""

    if (client1.getHasCurrentTurn()) {                                                            //client 1 turn
      try {
        val futureNextTurn = Future {
          game.evaluateMove(client1.chooseHouse(game), client1.ID)                                //returns who has the right to move
        }
        nextTurn = Await.result(futureNextTurn, Duration(30, TimeUnit.SECONDS))                   //wait for a result for 30 sec
      } catch {
        case _: TimeoutException => {
          println("\n" + client1.ID + ", your time is over. Move belongs to " + client2.ID)       //if doesn't move within 30 secons, its oponent's turn
          switchTurn(client1, client2)
          robot.keyPress(KeyEvent.VK_ENTER)
          playTurn(client1, client2)
        }
      }
      if (nextTurn != client1.ID) then switchTurn(client1, client2) //set who should start next turn
      if (game.checkIfOutOfStone(client1.ID)) then game.anounceWinner() else playTurn(client1, client2) //check if client finished


    } else { //client 2 turn
      try {
        val futureNextTurn = Future {
          game.evaluateMove(client2.chooseHouse(game), client2.ID)
        }
        nextTurn = Await.result(futureNextTurn, Duration(30, TimeUnit.SECONDS))
      } catch {
        case _: TimeoutException => {
          println(client2.ID + ", your time is over. Move belongs to " + client1.ID)
          switchTurn(client1, client2)
          robot.keyPress(KeyEvent.VK_ENTER)
          playTurn(client1, client2)
        }
      }
      if (nextTurn != client2.ID) then switchTurn(client1, client2) //set who should start next turn
      if (game.checkIfOutOfStone(client2.ID)){
        game.anounceWinner()
        playAgain()
      }
      else playTurn(client1, client2) //check if client finished
    }
  }


  def playAgain(): Unit = {
    println("\n\nType 'Y' to play again, 'X' to exit")
    var input = ""
    input = readLine
    input match {
      case "Y" | "y" | "yes" => start()
      case "X" | "x" => println("\nGoodbye!")
      case _ => playAgain()
    }
  }

  def showInstructions(): Unit = {
    println("Open link -> https://www.mastersofgames.com/rules/mancala-rules.htm\n\n")
    start()
  }


  def computerComputerGame() = {
    var computer1 = new ClientComputer(P1, true)
    var computer2 = new ClientComputer(P2, false)
    game = new Game(computer1.ID, computer2.ID)
    game.setUpNewBoard()
    playTurn(computer1, computer2)
  }

  def playerComputerGame() = {
    var player1 = new ClientPlayer(P1, true)
    var computer2 = new ClientComputer(P2, false)
    game = new Game(player1.ID, computer2.ID)
    game.setUpNewBoard()
    playTurn(player1, computer2)

  }

  def playerPlayerGame() = {
    var player1 = new ClientPlayer(P1, true)
    var player2 = new ClientPlayer(P2, false)
    game = new Game(player1.ID, player2.ID)
    game.setUpNewBoard()
    playTurn(player1, player2)
  }

  def switchTurn(client1: Client, client2: Client) = {
    client1.setHasCurrentTurn(!client1.getHasCurrentTurn());
    client2.setHasCurrentTurn(!client2.getHasCurrentTurn())
  }

}