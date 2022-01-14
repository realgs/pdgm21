import scala.util.Random

class Server(val player1: Player, val player2: Player):

  val game = new Kalaha()

  def play(): Unit =

    var movingPlayer:Player = player1
    var next = 0
    val rng = Random.nextInt()
    if rng % 2 == 1 then movingPlayer = player2

    while(!game.isOver){
      println("Player "+ movingPlayer.getId + " moving")
      game.printFields(movingPlayer.getId)
      next = game.move(movingPlayer.getId, movingPlayer.makeMove())
      if next == 1 then movingPlayer = player1
      else movingPlayer = player2

    }
    val winner = game.whoWon()
    println("GAME IS OVER!")
    if winner == 0 then println("It is a draw!!!\nCONGRATULATION")
    else println("\nand the winner is...\nPlayer " + winner + "!!!")
