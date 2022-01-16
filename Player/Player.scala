package Kalaha.Player

object Player {

  abstract class Player():
    def chooseNextMove(): Int

    def makeMove(lastMove: Int): Unit
}
