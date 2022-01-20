package pdgm21

import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.Random
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt
import pdgm21.Player
import pdgm21.Game

class Server:


  def play(player1:Player, player2:Player): Int = {

    /*
    * 0 - game ended without errors
    * -1  - player1 timeout
    * -2 - player2 timeot
    * -3 - player1 has id != 1
    *
    * */

    var game = new Game
    game.start()
    var gameState = new GameState(1, -2, game.gameBoard.clone())

    // this shouldn't happen
    if (player1.getId() != 1 || player2.getId() != 2) then return -3

    // main game loop
    while (!(gameState.moveEndCode == 1 || gameState.moveEndCode == 2)) {
      //      if(gameState.nextPlayer==1) then

      try {
        val makeMove1 = Future {
          var move1 = if gameState.nextPlayer == 1 then player1.makeMove(gameState) else player2.makeMove(gameState)
          gameState = game.move(move1, gameState.nextPlayer);
          while (gameState.moveEndCode == -1) do
            move1 = if gameState.nextPlayer == 1 then player1.makeMove(gameState) else player2.makeMove(gameState)
            gameState = game.move(move1, gameState.nextPlayer);
        end while
            println(s"Gracz${gameState.nextPlayer%2+1} wykonał ruch: $move1")
        }
        Await.result(makeMove1, 30.seconds)
      } catch {
        case e: TimeoutException => println(s"Przekroczono limit czasu dla gracza${gameState.nextPlayer}")
          return -1
      }
      game.printGameStatus()
    }
    if(gameState.moveEndCode==3) then println("Remis")
    else println(s"Wygral gracz${gameState.moveEndCode}")

    return 0
  }
  
