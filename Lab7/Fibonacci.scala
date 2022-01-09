package Lab7

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global


object Fibonacci {
  def fibonacci(n: Int):Int =
    if(n < 0)
      -1
    else
      n match {
        case 0 => 0
        case 1 => 1
        case _ => fibonacci(n-1) + fibonacci(n-2)
      }

  def fibonacciFuture(n: Int):Int =
    if(n < 0)
      -1
    else
      n match {
        case 0 => 0
        case 1 => 1
        case _ => {
          val f1 = Future{fibonacci(n-1)}
          val f2 = Future{fibonacci(n-2)}
          val el1 = Await.result(f1, Duration.Inf)
          val el2 = Await.result(f2, Duration.Inf)
          el1 + el2
        }
      }
}
