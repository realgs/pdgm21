package Kalaha

object Game {
  def main(args : Array[String]) : Unit = {
    val server = new Server
    server.startGame()
  }
}
