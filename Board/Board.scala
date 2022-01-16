package Kalaha.Board

object Board {

  abstract class Board():
    def copy(): Board

    def showBoard(): Unit

    def endOfGame(): Boolean

    def gamesEnd(): Unit

    def firstAvailableMoveA(): Int

    def firstAvailableMoveB(): Int

    def checkMoveA(move: Int): Int

    def checkMoveB(move: Int): Int

    def nextMoveA(move: Int): Boolean

    def nextMoveB(move: Int): Boolean
}
