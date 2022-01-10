package Fibonacci

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import ParallelFunction.ParallelFunction.*

object Fibonacci {

  def fibonacci(n: Int): Int =
    if n<0 then throw new Exception("Negative argument!")
    else
      n match {
        case 0 => n
        case 1 => n
        case _ => fibonacci(n-1) + fibonacci(n-2)
      }

  def fibonacciFuture(n: Int, threshold: Int): Int =
    if n<0 then throw new Exception("Negative argument!")
    else
      n match {
        case 0 => n
        case 1 => n
        case _ => if (n > threshold) {
                  val f1 = Future(fibonacciFuture(n-1, threshold))
                  val f2 = Future(fibonacciFuture(n-2, threshold))
                  val res = for {
                    x <- f1
                    y <- f2
                  } yield x+y
          Await.result(res, Duration.Inf)
        }
        else fibonacci(n-1) + fibonacci(n-2)
      }

  def fibonacciParallel(n: Int, threshold: Int): Int =
    if n<0 then throw new Exception("Negative argument!")
    else
      n match {
        case 0 => n
        case 1 => n
        case _ => if (n > threshold) {
          val (f1, f2) = parallel(fibonacciParallel(n-1, threshold), fibonacciParallel(n-2, threshold))
          f1 + f2
        }
        else fibonacci(n-1) + fibonacci(n-2)
      }
}
