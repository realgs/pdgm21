package pdgm21
import pdgm21.Game
import pdgm21.GameState
import pdgm21.Server
object Main {



  def runAuto(): Unit ={
    val server = new Server
    val player2 = new PlayerAI(server,2)
    val player1 = new PlayerHuman(server,1)
    server.aiVsHuman(player1,player2)
  }


  def debugScenario(): Unit ={
    val a = new Game()
    a.start()
    val gamestate =  a.move(0,1)
    a.printGameStatus()
    println(gamestate.gameBoard.toList)
  }
  def main(args :Array[String]): Unit = {
    runAuto()
      //debugScenario()
//    val a = new Game()
//    a.start()
//    println("podaj pole: ")
//    var field = scala.io.StdIn.readInt()
//    println("podaj gracza: ")
//    var player = scala.io.StdIn.readInt()
//    a.move(field, player)
//    a.printGameStatus()
//
//
//    while (player != -1) {
//      println("podaj pole: ")
//      field = scala.io.StdIn.readInt()
//      println("podaj gracza: ")
//      player = scala.io.StdIn.readInt()
//      a.move(field, player)
//      a.printGameStatus()
//
//
//    }
  }
}
