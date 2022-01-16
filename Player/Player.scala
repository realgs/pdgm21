package Kalaha.Player

object Player {

  abstract class Player():
    def getNextMove(): Int

    def makeMove(lastMove: Int): Unit
}
