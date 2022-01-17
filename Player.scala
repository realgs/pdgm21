package KalahaGame
import KalahaGame._

abstract class Player {
    def makeMove(board: Board): Int
}
