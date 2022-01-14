package pdgm21
import pdgm21.Game
import pdgm21.GameState
import pdgm21.Server

import scala.collection.mutable.ArrayBuffer
object Main {
  
  def main(args :Array[String]): Unit = {
    val server  = new Server()
    val player1 = new PlayerHuman(server,1)
    val player2 =new  PlayerAI(server,2)
    server.play(player1,player2)
  }
}

