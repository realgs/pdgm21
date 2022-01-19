import scala.io.StdIn.readInt

object Main {

  def main(args: Array[String]): Unit =
    val server = new Server()

    menu()

    var choice = readInt()

    while (choice < 1 || choice > 6)
      {
        println("Wrong input, try again!")
        menu()
        choice = readInt()
      }

    choice match {
      case 1 => {
        val player1 = new HumanPlayer(server, 1)
        val player2 = new HumanPlayer(server, 2)
        server.playGame(player1, player2)
      }
      case 2 => {
        val player1 = new HumanPlayer(server, 1)
        val player2 = new StrongEngine(server, 2)
        server.playGame(player1, player2)
      }
      case 3 => {
        val player1 = new HumanPlayer(server, 1)
        val player2 = new RandomComputer(server, 2)
        server.playGame(player1, player2)
      }
      case 4 => {
        val player1 = new StrongEngine(server, 1)
        val player2 = new StrongEngine(server, 2)
        server.playGame(player1, player2)
      }
      case 5 => {
        val player1 = new StrongEngine(server, 1)
        val player2 = new RandomComputer(server, 2)
        server.playGame(player1, player2)
      }
      case 6 => {
        val player1 = new RandomComputer(server, 1)
        val player2 = new RandomComputer(server, 2)
        server.playGame(player1, player2)
      }
    }

  def menu() =
    println("Choose a game mode:")
    println("1 - player vs player")
    println("2 - player vs strong computer")
    println("3 - player vs weak computer")
    println("4 - strong computer vs strong computer")
    println("5 - strong computer vs weak computer")
    println("6 - weak computer vs weak computer")

}
