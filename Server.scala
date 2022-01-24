import Server.{AskForMove, EndGame, MakeMove, Move}
import akka.actor.{Actor, ActorRef, Terminated}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, TimeoutException}

class Server(val player1: ActorRef, val player2: ActorRef, val board: Board) extends Actor {

  override def receive: Receive = {

    case AskForMove => askForMove()
    case Move(fieldNumberFuture) => makeMove(waitForMove(fieldNumberFuture))
    case EndGame => endGame()
  }

  private def askForMove(): Unit = {

    printf("%n%n# PLAYER %d.", if (board.isPlayer1Move) 1 else 2)
    if (board.isPlayer1Move) player1 ! MakeMove(board)
    else player2 ! MakeMove(board)
  }

  private def makeMove(fieldNumber: Int): Unit = {

    if(fieldNumber == -1) {

      printf("%n%nTIME IS UP! PLAYER %d. WON!%n%n# THE GAME IS OVER%n", if (board.isPlayer1Move) 2 else 1)
      context.system.terminate()

    } else {

      printf("%nPLAYER %d. HAS CHOSEN %d%n", if (board.isPlayer1Move) 1 else 2, fieldNumber)
      board.makeMove(fieldNumber)
      board.print()
      if (board.isGameOver) self ! EndGame
      else self ! AskForMove
    }
  }

  private def waitForMove(fieldNumberFuture: Future[Int]): Int = {

    try {

      Await.result(fieldNumberFuture, 30.seconds)

    } catch {

      case _: TimeoutException => fieldNumberFuture.failed; -1
    }
  }

  private def endGame(): Future[Terminated] = {

    println("\n\nTHE GAME IS OVER")
    board.printResult()
    context.system.terminate()
  }
}

object Server {

  case object AskForMove
  case class MakeMove(board: Board)
  case class Move(fieldNumberFuture: Future[Int])
  case object EndGame
}