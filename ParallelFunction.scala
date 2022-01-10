package ParallelFunction

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object ParallelFunction {

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right: Future[B] = Future {
      taskB
    }
    val left: A = taskA
    (left, Await.result(right, Duration.Inf))
}

