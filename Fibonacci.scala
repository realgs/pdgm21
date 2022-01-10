import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Fibonacci {

  def fibonacci(n :Int): Int =
    n match
      case 1 => 1
      case 2 => 1
      case _ => fibonacci(n-1) + fibonacci(n-2)


  def fibonacciParallel(n :Int, max: Int): Int =
    fibonacciParallelSplit(n, 0, max)



  def fibonacciParallelSplit(n :Int, current: Int, max: Int): Int =
    if(current < max - 1) then
      n match
        case 1 => 1
        case 2 => 1
        case _ =>
          //split
          val fib1 = Future(fibonacciParallelSplit(n-1,current + 2, max) )
          val fib2 = Future(fibonacciParallelSplit(n-2,current + 2,max) )

          //wait for results
          val result1 = Await.result(fib1, Duration.Inf)
          val result2 = Await.result(fib2, Duration.Inf)
          
          //return
          result1 + result2
    else
      n match
        case 1 => 1
        case 2 => 1
        case _ => fibonacci(n-1) + fibonacci(n-2)




}
