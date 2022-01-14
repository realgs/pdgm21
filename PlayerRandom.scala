package pdgm21

import scala.util.Random

class PlayerRandom(var server:Server,playerId:Int) extends Player:
  override def makeMove(gameState: GameState): Int ={
    if getId()==2 then 7+Random.nextInt(6)
    else
      Random.nextInt(6)
  }

  override def getId(): Int = {
    playerId
  }