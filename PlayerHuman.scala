package pdgm21

import scala.util.Random


class PlayerHuman(var server: Server,playerId:Int) extends Player:

  override def makeMove(gameState: GameState): Int ={
    println("podaj pole")
    val move = scala.io.StdIn.readInt()
    return move
  }

  override def getId():Int={
    return playerId
  }

