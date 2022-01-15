import scala.util.Random

class Server(private val player1: Player,private val player2: Player,private val game: Kalaha):

  def play(): Unit =

    var movingPlayer:Player = player1
    var next = 0
    val rng = Random.nextInt()
    if rng % 2 == 1 then movingPlayer = player2

    while(!game.isOver()){
      println("Player "+ movingPlayer.getId + " moving")
      game.printFields(movingPlayer.getId)
      val choice = movingPlayer.makeMove()
      next = game.move(movingPlayer.getId, choice)
      println("Field " + (choice + 1) + " chosen")
      if next == 1 then movingPlayer = player1
      else movingPlayer = player2

    }
    game.printFields(movingPlayer.getId)
    val winner = game.whoWon()
    println("GAME IS OVER!")
    if winner == 0 then println("It is a draw!!!\nCONGRATULATION")
    else println("\nand the winner is...\nPlayer " + winner + "!!!")
