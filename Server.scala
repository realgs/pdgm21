package pdgm21

import scala.util.Random

class Server {

// TODO implement time counting probably use parallel computing for that


  def aiVsAi(player1: PlayerAI,player2: PlayerAI): Int ={
    var game =  new Game
    game.start()
    var gameState= new GameState(1,-2,null)
    if (player1.getId() != 1 || player2.getId() != 2) then return -2
    while (!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)){
      if(gameState.nextPlayer==1) then
        val move = player1.makeMove()
        gameState = game.move(move,player1.getId())
      else
        val move = player2.makeMove()
        gameState = game.move(move,player2.getId())

      game.printGameStatus()
    }
    println(s"Wygral gracz${gameState.moveEndCode}")






    return 0


  }





  def gameLoop(): Unit ={

    var game = new Game
    game.start()
    var gameState = new GameState(1,-2,null)
    var moveCounter=0
    while(!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)) {
      // TODO change for client - serwer style
      if(gameState.nextPlayer==1)
        val move = Random.nextInt(6)
        //println(s"player:${gameState.nextPlayer} ruch:$move  kod: ${gameState.moveEndCode} ")
        gameState = game.move(move,1)
      else
        val move = 7+ Random.nextInt((12-7)+1)
        //println(s"player:${gameState.nextPlayer} ruch:$move  kod: ${gameState.moveEndCode} ")
        gameState = game.move(move,2)

      game.printGameStatus()
    }

    println(s"Wygrał: gracz${gameState.moveEndCode}")

  }




}
