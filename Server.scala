import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeUnit

class Server(private val player1: Player,private val player2: Player,private val game: Kalaha,private val time: Int):

  def play(): Int =

    var movingPlayer:Player = player1
    var next = 1
    val rng = Random.nextInt()
    if rng % 2 == 1 then movingPlayer = player2
                         next = 2

    while(!game.isOver() ){
      println("Player "+ movingPlayer.getId + " moving")
      game.printFields(movingPlayer.getId)
      try{
        val FutureChoice = Future(movingPlayer.makeMove(time))
        val choice = Await.result(FutureChoice,Duration(time, TimeUnit.SECONDS))

        if choice < 0 || choice > 5  then
          println(choice + " is a wrong move! You loose your turn")
          if next == 1 then next = 2
          else next = 1
        else
          if next == 1 then player2.getEnemyMove(choice)
          else player1.getEnemyMove(choice)
          next = game.move(movingPlayer.getId, choice)
          println("Field " + (choice + 1) + " chosen")
      }
      catch{
        case e:java.util.concurrent.TimeoutException => println("Czas minal!")
          return 4

      }
      if next == 1 then movingPlayer = player1
      else movingPlayer = player2

    }
    game.printFields(movingPlayer.getId)
    val winner = game.whoWon()
    println("GAME IS OVER!")
    if winner == 0 then println("It is a draw!!!\nCONGRATULATION")
    else println("\nand the winner is...\nPlayer " + winner + "!!!")
    0
