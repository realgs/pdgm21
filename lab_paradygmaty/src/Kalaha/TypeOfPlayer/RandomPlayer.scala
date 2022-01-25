package Kalaha.TypeOfPlayer

import scala.util.Random
import Kalaha.Board
import Kalaha.TypeOfPlayer.Player

class RandomPlayer(val board: Board) extends Player {

  override def move(): Int = {
    var choice = Random.nextInt(board.secondBase)
    if board.checkCorrectnessOfField(choice) then
      println("Wybór randomPlayera: " + choice)
      choice
    else move()
  }
}

