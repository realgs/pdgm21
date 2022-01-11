package pdgm21

import scala.util.Random

class PlayerHuman(var server: Server,playerId:Int):
  def makeMove(): Int ={
    println("poodaj pole")
    val move = scala.io.StdIn.readInt()
    return move

  }

  def getId():Int={
    return playerId
  }

