package kalahgame

import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

class Server:

  private var activePlayer: Player = _
  private var inactivePlayer: Player = _
  private var turn = 1
  private var gameOver = false

  private def changePlayers(bonusTurn: Boolean, player1: Player, player2: Player): Unit =
    if (activePlayer == player1 && !bonusTurn) then
      activePlayer = player2
      inactivePlayer = player1
    else if (activePlayer == player2 && !bonusTurn) then
      activePlayer = player1
      inactivePlayer = player2
    else
      println("\t\t\t\t\t\t\t\t\tPlayer" + activePlayer.ID + " has a bonus turn.")

  def runKalah(player1: Player, player2: Player): Unit =
    val board = new Board

    player1.ID = 1
    player2.ID = 2

    val rand = new scala.util.Random

    if(rand.nextInt(10) < 5)
      activePlayer = player1
      inactivePlayer = player2
    else
      activePlayer = player2
      inactivePlayer = player1

    println("\n\t\t\t\t\t ## KALAH ## \n\t\t\t\t\t- Good luck -")
    println("<><><><><><><><><><><><><><><><><><><><><><><><><><><>")
    while
      !gameOver
    do
      println("\n\t***********************************************")
      println(s"\t\t\t\t\tTurn number: $turn")
      println("\t***********************************************")
      board.printBoard()

      var selectedPit = -1
      var timeout = false

      try {
        val move = Future {
          selectedPit = activePlayer.pickMove(board)
        }
        Await.result(move, 30.seconds)
      } catch{

        case e: TimeoutException => println(s"\t\t\t\t\t\nTime limit has been exceeded by player${activePlayer.ID} !")
          timeout = true
      }

      if (board.validSelection(selectedPit, activePlayer.ID)) then
        var bonusTurn = board.makePlay(selectedPit, activePlayer.ID)
        if(board.checkWin())
          println("\n\n\t\t\tThe game is finished after " + turn + " turns!")
          val winnerId = board.winner
          if (winnerId != -1)
            println(s"\t\t\t	!!Player$winnerId Wins The Game!!")
            gameOver = true
          else
            println("\t\t\t\t\t!! IT'S A DRAW !!")
            gameOver = true
          board.printBoard()
          println("\t\t\t\t\t- Game End -")
        else
          changePlayers(bonusTurn, player1, player2)
          turn += 1
      else if(timeout) then
        gameOver = true
        println("\n\n\t\t\tThe game is finished after " + turn + " turns!")
        println(s"\t\t\t\t!!Player${inactivePlayer.ID} Wins by walk-over!!")
        board.interruptedSumUp()
        println("\t\t\t\t\t- Game End -")
      else
        println("\t\t\t\t\t\t\t\t\tInvalid input - please select a pit index that is not empty!")

end Server