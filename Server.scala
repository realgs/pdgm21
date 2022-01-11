package pdgm21

import scala.concurrent.{Await, Future}
import scala.util.Random
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt


class Server {



// TODO implement time counting probably use parallel computing for that

  def aiVsHuman(player1:PlayerHuman , player2:PlayerAI): Int ={
    var game =  new Game
    game.start()
    var gameState= new GameState(1,-2,null)
    if (player1.getId() != 1 || player2.getId() != 2) then return -2
    while (!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)){
      if(gameState.nextPlayer==1) then
        println("gracz1")
        val makeMove1 =Future{
          val move = player1.makeMove();
          gameState = game.move(move,player1.getId());
          // pyta o kolejny ruch do momentu az bedzie poprawny albo czas sie skonczy
          while (gameState.moveEndCode== -1){
            println("Błędny ruch ")
            val move = player1.makeMove();
            gameState = game.move(move,player1.getId());
          }
        }
        Await.result(makeMove1,3.seconds)

      else
        println("gracz2")
        val makeMove2 =Future{
          val move = player2.makeMove();
          gameState = game.move(move,player2.getId());
          while (gameState.moveEndCode== -1){
            val move = player2.makeMove();
            gameState = game.move(move,player2.getId());
          }
        }
        Await.result(makeMove2,2.seconds)
      game.printGameStatus()
    }
    println(s"Wygral gracz${gameState.moveEndCode}")

    return 0

  }
  def aiVsAi(player1: PlayerAI,player2: PlayerAI): Int ={
    var game =  new Game
    game.start()
    var gameState= new GameState(1,-2,null)
    if (player1.getId() != 1 || player2.getId() != 2) then return -2
    while (!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)){



      if(gameState.nextPlayer==1) then
        println("gracz1")
        val makeMove1 =Future{
          val move = player1.makeMove();
          gameState = game.move(move,player1.getId());
          // pyta o kolejny ruch do momentu az bedzie poprawny albo czas sie skonczy
          while (gameState.moveEndCode== -1){
            val move = player1.makeMove();
            gameState = game.move(move,player1.getId());
          }
        }
        Await.result(makeMove1,3.seconds)

      else
        println("gracz2")
        val makeMove2 =Future{
          val move = player2.makeMove();
          gameState = game.move(move,player2.getId());
          while (gameState.moveEndCode== -1){
            val move = player2.makeMove();
            gameState = game.move(move,player2.getId());
          }
        }
        Await.result(makeMove2,2.seconds)
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
