import javax.sound.midi.Receiver
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import concurrent.duration.DurationInt

class Server{

  val time = 20
  var board = new Board
  var player1 : Player = _
  var player2 : Player = _

  def chooseGame() : Unit = {
    println("Wybierz rozgrywke: ")
    println("(1) uzytkownik - uzytkownik")
    println("(2) uzytkownik - komputer")
    println("(3) komputer - komputer")

    var choice = scala.io.StdIn.readInt()

    if (choice >= 1 && choice <=3){
      choice match {
        case 1 =>
          player1 = new User(board)
          player2 = new User(board)

        case 2 =>
          player1 = new User(board)
          player2 = new Computer(board)

        case 3 =>
          player1 = new Computer(board)
          player2 = new Computer(board)
      }
      choosePlayer()

    } else {
      println("Bledny numer")
      chooseGame()
    }
  }

  def choosePlayer() : Unit = {
    println("Ktory gracz rozpoczyna gre?")
    println("(1) - gracz 1")
    println("(2) - gracz 2")

    var choice1 = scala.io.StdIn.readInt()

    if (choice1 == 1 || choice1 == 2){
      choice1 match {
        case 1 => board.setActivePlayer(true)
        case 2 => board.setActivePlayer(false)
      }
    } else {
      println("Bledny numer")
      choosePlayer()
    }
  }

  def startGame() : Unit = {
    chooseGame()
    game()
  }

  def game() : Unit = {
    board.printBoard()
    while (board.isNextMovePossibe()) {
      try {
        val moveTime = Future {
          if (board.findActivePlayer() && board.isNextMovePossibe()) {
            println("Gracz 1: ")
            val index = player1.move()
            if (board.checkChoice(index)) {
              board.moveSeedsFrom(index)
              board.nextPlayer()
              game()
            } else {
              println("Wybierz ponownie pole")
              game()
            }
          } else if (!board.findActivePlayer() && board.isNextMovePossibe()) {
            println("Gracz 2: ")
            val index = player2.move()
            if (board.checkChoice(index)) {
              board.moveSeedsFrom(index)
              board.nextPlayer()
              game()
            } else {
              println("Wybierz ponownie pole")
              game()
            }
          } else if (!board.isNextMovePossibe()) {
            endGame()
          }
        }
        Await.result(moveTime, time.second)
      } catch {
        case _: TimeoutException => println("Przekroczono limit czasu")
          return false
      }
    }

  }

  def endGame() : Unit = {
    board.endResult()
  }
}


