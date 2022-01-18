package Player

import Player.*
import Game.Game.*

object Human {
  class Human(game: Game) extends Player(game) {

    def makeMove(hole: Int): Boolean = {
      splitStones(hole)
    }
  }
}
