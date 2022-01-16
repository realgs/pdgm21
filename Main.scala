package Kalaha

import Kalaha.Server.*

object Main {

  def main(args: Array[String]): Unit =

    val server = new Server()
    server.runGame()
}
