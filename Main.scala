object Main:
  def main(args: Array[String]): Unit = {
    val player1 = new HumanConsolePlayer()
    val player2 = new HumanConsolePlayer()
    val server = new Server(player1, player2)
    server.play(6, 4)
  }
