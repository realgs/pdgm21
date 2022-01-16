package Kalaha.User

import Kalaha.Gameboard.Game_board


class Player_human(val game: Game_board) extends Player {

  override def make_move(): Int =
    println("Podaj pole od 0")

    val number = scala.io.StdIn.readInt()
    number
}
