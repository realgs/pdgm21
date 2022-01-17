import java.util.concurrent.TimeUnit
import java.util.{Timer, TimerTask}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.io.StdIn.readLine
import scala.util.{Failure, Success}


class Server {
  private var kalah: Kalah = _
  private var human: Client = _
  private var human2: Client = _
  private var playerAI_1: Client = _
  private var playerAI_2: Client = _
  private var isChoosen: Boolean = true

  val maxTime:FiniteDuration = {
    Duration(30, TimeUnit.SECONDS)
  }
  

  def explanation():Unit = {
    var choice = 0
    println("WELCOME TO KALAHA")
    println("Choose game option: \n 1) computer vs computer \n 2) player vs computer \n 3) player vs player")
    choice = readLine.toInt
    print("choice: ")
    println(choice)
    if (choice == 1 || choice == 2 || choice == 3) {
      if (choice == 1) then compVScomp()
      else if (choice == 2) then playerVScomp()
      else playVSplay()
    } else {
      println("Wrong number! Try again")
      explanation()
    }
  }

  def compVScomp():Unit = {
    playerAI_1 = new ClientAI("player1", true)
    playerAI_2 = new ClientAI("player2", false)
    kalah = new Kalah(playerAI_1.name, playerAI_2.name)
    kalah.createBoard()
    playGame(playerAI_1, playerAI_2)
  }

  def playerVScomp(): Unit = {
    human = new ClientHuman("player1", true)
    playerAI_2 = new ClientAI("player2", false)
    kalah = new Kalah(human.name, playerAI_2.name)
    kalah.createBoard()
    playGame(human, playerAI_2)

  }

  def playVSplay() = {
    human = new ClientHuman("player1", true)
    human2 = new ClientHuman("player2" ,false)
    kalah = new Kalah(human.name, human2.name)
    kalah.createBoard()
    playGame(human, human2)
  }


  def playGame(player1: Client, player2: Client): Unit = {
    kalah.printBoard()

    implicit val ec = ExecutionContext.global

    var whoseMove = ""
    println("PLAY GAME" )
    if (player1.isFirst) {
      try {
        val result = Future {
          kalah.makeMove(player1.chooseField(kalah), player1.name)
        }
        whoseMove = Await.result(result, maxTime)
      } catch {
        case _: TimeoutException => {
          println("Time is over")
        }
      }


      if (whoseMove == player1.name) then
        player1.setIsFirst(true)
        player2.setIsFirst(false)
      else
        player2.setIsFirst(true)
        player1.setIsFirst(false)

      if kalah.checkIfEnd(player1.name) == true then endGame()
      else if whoseMove != "" then playGame(player1, player2)
      else stopGame()

    } else if (player2.isFirst) {
      try {
        val result = Future {
          kalah.makeMove(player2.chooseField(kalah), player2.name)
        }
        whoseMove = Await.result(result, maxTime)
      } catch {
        case _: TimeoutException => {
          println("Time is over")
        }
      }

      if (whoseMove == player1.name) then
        player1.setIsFirst(true)
        player2.setIsFirst(false)
      else
        player2.setIsFirst(true)
        player1.setIsFirst(false)

      if kalah.checkIfEnd(player1.name) == true then endGame()
      else if whoseMove == "" then stopGame()
      else playGame(player1, player2)

    }

  }

  def endGame() = {
    kalah.whoWins()
  }

  def stopGame(): Unit = {
    println("END OF THE GAME")
  }


}
