import server.Server

object StartGame {
  def main(args: Array[String]): Unit = {
    val server = Server()
    server.startGUI()
  }
}
