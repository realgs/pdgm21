package game

import boards.Board.Board
import players.Player.{Player, SimulationPlayer}

object Game {
  def playing(board: Board, player1: Player, player2: Player): Unit ={
    board.setBoard()
    var round =0;
    var test =0;
    while(!board.endOfTheGame()||test==2){
      test+=1
      var x = false
      println(s"ROUND $round")
      println("stones: "+board.checkSumOfStones())
      if(round%2==0){
        var check = player1.move()
        while(board.Player1Row(check)==0){
          check = player1.move()
        }
        println("Move Player1: "+check)
        x =board.movingPieces(check, 1)
      }
      else{
        var check = player2.move()
        println("DOUBLE CHECK: "+board.Player2Row(check))
        while(board.Player2Row(check)==0){
          check = player2.move()
        }
        println("Move Player2: "+check)
        x =board.movingPieces(check, 2)
      }
      if x==false then round+=1
      board.printBoard()
    }
    board.gameOver()
  }

  def Simulation(): Unit ={
    var board = new Board()
    var player1 = new SimulationPlayer(board, 1)
    var player2 = new SimulationPlayer(board, 2)
    playing(board, player1, player2)
  }

}
