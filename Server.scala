
import concurrent.duration.{Duration, DurationInt}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException, blocking}

class Server {
  var board = new Board
  var player1: Player = _
  var player2: Player = _

  def start(): Unit ={
    play()
    makeMove(player1)
  }
  def menu(): Unit = {
    println("Menu\n" +
      "1. player vs player\n" +
      "2.player vs comp\n" +
      "3.comp vs pl\n" +
      "4.comp vs comp\n" +

      "Choos option:")
  }

  def play(): Unit = {
    menu()
    var choice = 0
    try {
      choice = scala.io.StdIn.readInt()
    } catch {
      case _: Exception => choice = 0
    }

    choice match {
      case 1 =>
        player1= new Human()
        player2 = new Human()
      case 2 =>
        player1 = new Human()
        player2 = new Computer()
      case 3 =>
        player1 = new Computer()
        player2 = new Human()
      case 4 =>
        player1 = new Computer()
        player2 = new Computer()
      case _ =>
        println("Wrong input")
        play()
    }
    makeMove(player1)
  }

  def makeMove(player: Player): Unit ={
    try {
      val move = Future{
        while (!board.isGameEnd){
          board.printBoard()
          val choice = player.makeMove(board)
          if board.isMoveFirstPlayer then println(s"\n 1. player choice: $choice")
          else println(s"\n player 2 choice: $choice")
          board.MakeMove(choice)
          if board.isCanMoreMove then makeMove(player)
          else if board.isMoveFirstPlayer then makeMove(player1)
          else makeMove(player2)
        }
      }
       Await.result(move, Duration.Inf)
    } catch {
      case e: TimeoutException => println("Time out")
        board.isGameEnd = true
        board.printScore()
    }
  }

}
