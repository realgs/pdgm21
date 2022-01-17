package KalahaGame
import KalahaGame.*

import scala.concurrent.{Await, Future, TimeoutException}
import concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

class HumanPlayer extends Player {

  def makeMove(board: Board): Int = {
    print("\nChoose a pit from 1 to 6: ")
//    try {
//      val move = Future {
        val choice = scala.io.StdIn.readInt()
        choice
//      }
//      Await.result(move, 10.seconds)
//    } catch {
//      case e: TimeoutException => println("\n!! TIME OUT !! Try again")
//        makeMove(board)
//    }

  }
}
