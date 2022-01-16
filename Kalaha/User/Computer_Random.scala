package Kalaha.User

import Kalaha.Decision_Three.Engine
import Kalaha.Gameboard.Game_board
import Kalaha.Server.Server

import scala.util.Random

class Computer_Random(val game: Game_board, val server: Server) extends Player {
  override def make_move(): Int =
    var random = Random.nextInt(game.NUMBER_HOUSES)
    var helper1 = true
    while helper1 do
      random = Random.nextInt(game.NUMBER_HOUSES)
      if !game.is_legal_move(random, server.player_move) then print("") else helper1 = false

    random


}
