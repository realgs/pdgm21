import akka.actor.Actor

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn

class HumanPlayer extends Actor {

  override def receive: Receive = {

    case Server.MakeMove(board: Board) =>
      sender ! Server.Move(Future{makeMove(board)})
  }

  private def makeMove(board: Board): Int = {

    var fieldNumber: Int = 0

    while(!board.ifCorrectFieldNumber(fieldNumber)) {

      print("\n\n# CHOSE FIELD (1-6): ")

      try {

        fieldNumber = StdIn.readInt()

        if(!board.ifCorrectFieldNumber(fieldNumber))
          println("\n# WRONG FIELD NUMBER! TRY AGAIN.")

      } catch {

        case _: NumberFormatException => println("\n# WRONG INPUT! TRY AGAIN.")
        case _: Exception => ()
      }
    }

    fieldNumber
  }
}