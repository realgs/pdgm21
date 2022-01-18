package Player

import Game.Game.*

import scala.annotation.tailrec

object Player {

  abstract class Player(var game: Game) {
    
    def makeMove(hole: Int): Boolean

    def splitStones(startHole: Int): Boolean = {
      val stones = game.board(startHole)
      game.board(startHole) = EMPTY_HOLE
      @tailrec
      def split(stonesToSplit: Int, current: Int): Boolean = {
        var currentHole = current % HOLES_N
        if game.isOpponentMancala(current) then currentHole = (current + 1) % HOLES_N
        game.board(currentHole) += 1
        stonesToSplit match
          case 0 => 
            if !game.isEnd() && game.checkExtraMove(currentHole) then return true  //returns true, when extra move possible
            else if game.canSteal(currentHole) then game.stealStones(currentHole)
            false
          case _ => split(stonesToSplit - 1, currentHole + 1)
      }
      split(stones - 1, startHole + 1)
    }
  }

}
