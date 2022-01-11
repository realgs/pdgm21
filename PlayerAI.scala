package pdgm21

import scala.util.Random

class PlayerAI(var server: Server,playerId:Int):


  def makeMove(): Int ={

    //basic implementation hope to make something better tho
    if(playerId==1)then Random.nextInt(6)
    else 7+ Random.nextInt((12-7)+1)

  }

  def getId():Int={
      return playerId
  }


