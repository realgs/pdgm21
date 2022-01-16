package Kalaha

import Kalaha.Gameboard.Game_board
import Kalaha.Server.Server

object Main {


  def main(args: Array[String]): Unit = {

    val server = new Server()
    server.game()

  }

}
