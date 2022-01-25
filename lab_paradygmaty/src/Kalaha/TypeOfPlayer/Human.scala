package Kalaha.TypeOfPlayer

import Kalaha.Board
import Kalaha.TypeOfPlayer.Player

class Human(val board: Board) extends Player {

  override def move(): Int = {
    println("Podaj swój ruch")
    var choice = -1
    try {
      choice = scala.io.StdIn.readInt()
    } catch {
      case _ : IllegalArgumentException => "wprowadzono nieprawidłowy rodzaj danych"
    }
    if (board.checkCorrectnessOfField(choice)) choice
    else {
      println("Błędny ruch, spróbuj raz jeszcze")
      move()
    }
  }

}
