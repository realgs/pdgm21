package Client
import Game.Board
import scala.io.StdIn.*


class Player(name: String, number: Int) extends Client(name, number) {
  private val PLAYER1 = 1
  private val PLAYER2 = 2
  override def whichHole(board: Board): Int =
    var hole: Int = readInt()
    var index: Int = if number == PLAYER1 then hole - 1
                     else hole + 6
    while ((hole < 1 || hole > 6) || board.emptyHole(index))
    {
      println("Wybrano niepoprawny numer : Taki dołek nie istnieje lub jest pusty")
      println("Podaj numer jeszcze raz: ")
      hole = readInt()
      index = if number == PLAYER1 then hole - 1
              else hole + 6
    }
    hole


}

