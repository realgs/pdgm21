package pdgm21

import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.Random
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt
import pdgm21.Player
import pdgm21.Game

class Server:


  def play(player1:Player, player2:Player): Int ={

    /*
    * 0 - game ended without errors
    * -1  - player1 timeout
    * -2 - player2 timeot
    * -3 - player1 has id != 1
    *
    * */
    
    var game =  new Game
    game.start()
    var gameState= new GameState(1,-2,game.gameBoard.clone())

    // this shouldn't happen
    if (player1.getId() != 1 || player2.getId() != 2) then return -3

    // main game loop
    while (!(gameState.moveEndCode== 1|| gameState.moveEndCode==2)){
      if(gameState.nextPlayer==1) then

        // server waits for player to move for given amount of time
        try{
          val makeMove1 =Future{
            var move1 = player1.makeMove(gameState);
            gameState = game.move(move1,player1.getId());
            while (gameState.moveEndCode== -1)do
              move1 = player1.makeMove(gameState);
              gameState = game.move(move1,player1.getId());
            end while
            println(s"Gracz1 wykonał ruch: $move1")
          }
          Await.result(makeMove1,30.seconds)
        }catch {
          case e: TimeoutException=> println("Przekroczono limit czasu dla gracza1")
            return -1
        }

      else

        try{
          val makeMove2 =Future{
            var move2 = player2.makeMove(gameState);
            gameState = game.move(move2,player2.getId());

            while (gameState.moveEndCode== -1)do
              move2 = player2.makeMove(gameState);
              gameState = game.move(move2,player2.getId());
            end while
            println(s"Gracz2 wykonał ruch: $move2")
          }
          Await.result(makeMove2,30.seconds)

        }catch {
          case e: TimeoutException=> println("Przekroczono limit czasu dla gracza2")
            return -2
        }
      game.printGameStatus()
    }
    //printing game result if ended
    if(gameState.moveEndCode==3) then println("Remis")
    else println(s"Wygral gracz${gameState.moveEndCode}")

    return 0
  }
  
