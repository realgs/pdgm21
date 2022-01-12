package pdgm21

import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.Random
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt
import pdgm21.Player
import pdgm21.Game

class Server {

// TODo handle outoftime exception nicely




  def play(player1:Player, player2:Player): Int ={

    var game =  new Game
    game.start()
    var gameState= new GameState(1,-2,null)

    if (player1.getId() != 1 || player2.getId() != 2) then return -2

    // main game loop
    while (!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)){
      if(gameState.nextPlayer==1) then
        println("gracz1")

        // server waits for player to move for 3 seconds
        try{
          val makeMove1 =Future{
            val move = player1.makeMove();
            gameState = game.move(move,player1.getId());
            while (gameState.moveEndCode== -1){
              val move = player1.makeMove();
              gameState = game.move(move,player1.getId());
            }
          }
          Await.result(makeMove1,3.seconds)
        }catch {
          case e: TimeoutException=> println("Przekroczono limit czasu dla gracza1")
            return -2
        }

      else
        println("gracz2")

        try{
          val makeMove2 =Future{
            val move = player2.makeMove();
            gameState = game.move(move,player2.getId());
            while (gameState.moveEndCode== -1){
              val move = player2.makeMove();
              gameState = game.move(move,player2.getId());
            }
          }
          Await.result(makeMove2,2.seconds)
        }catch {
          case e: TimeoutException=> println("Przekroczono limit czasu dla gracza2")
            return -2
        }

      game.printGameStatus()
    }

    //printing game result
    if(gameState.moveEndCode==3) then println("Remis")
    else println(s"Wygral gracz${gameState.moveEndCode}")

    return 0

  }



  // also legacy
  def aiVsAi(player1: PlayerAI,player2: PlayerAI): Int ={
    var game =  new Game
    game.start()
    var gameState= new GameState(1,-2,null)
    if (player1.getId() != 1 || player2.getId() != 2) then return -1
    while (!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)){

      if(gameState.nextPlayer==1) then
        println("gracz1")
        try{
          val makeMove1 =Future{
            val move = player1.makeMove();
            gameState = game.move(move,player1.getId());
            while (gameState.moveEndCode== -1){
              val move = player1.makeMove();
              gameState = game.move(move,player1.getId());
            }
          }
          Await.result(makeMove1,3.seconds)
        }catch {
          case e: TimeoutException=> println("Przekroczono limit czasu dla gracza1")
            return -2
        }


      else
        println("gracz2")
        try{
          val makeMove2 =Future{
            val move = player2.makeMove();
            gameState = game.move(move,player2.getId());
            while (gameState.moveEndCode== -1){
              val move = player2.makeMove();
              gameState = game.move(move,player2.getId());
            }
          }
          Await.result(makeMove2,2.seconds)
        }catch {
          case e: TimeoutException=> println("Przekroczono limit czasu dla gracza2")
            return -2
        }
      game.printGameStatus()
    }

    if(gameState.moveEndCode==3) then println("Remis")
    else  println(s"Wygral gracz${gameState.moveEndCode}")

    return 0


  }




//legacy
  def gameLoop(): Unit ={

    var game = new Game
    game.start()
    var gameState = new GameState(1,-2,null)
    var moveCounter=0
    while(!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)) {
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
