package Kalaha.User

import Kalaha.Decision_Three.Engine
import Kalaha.Gameboard.Game_board
import Kalaha.Server.Server

import scala.util.Random

class Computer_Pro(val game: Game_board, val server: Server) extends Player {
  override def make_move(): Int =
    val eng = new Engine(server.player_move, 5, server)
    eng.best_move()


}
