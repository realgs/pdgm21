import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf


object L7 {

  // TIME COUNTER
  def time[A](fun: => A): Unit = {
    val startTime = System.nanoTime()
    fun
    val stopTime = System.nanoTime()
    println("Time equals: " + (stopTime - startTime) /1000000 + " ms")
  }

  // FIBONACCI

  def fib(n:BigInt) : BigInt =
    n match
    {
      case 0 => 0
      case 1 => 1
      case _ => if!(n <= 0) then return fib(n-2) + fib(n-1)
      else throw new Exception ("n less than zero")
    }

  def fibParallel(n:BigInt) : BigInt =
    n match
    {
      case 0 => 0
      case 1 => 1
      case _ => if!(n <= 0) then
        val f1 = Future(fib(n-1))
        val f2 = Future(fib(n-2))
        return Await.result(f1, Inf) + Await.result(f2, Inf)
      else throw new Exception ("n less than zero")
    }


  def main(args: Array[String]): Unit = {

    //FIBONACCI TESTS
    println("Fibonacci sequence 20 without parallel: ")
    time(fib(20))
    println("Fibonacci sequence 20 parallel: ")
    time(fibParallel(20))

    println("Fibonacci sequence 25 without parallel: ")
    time(fib(25))
    println("Fibonacci sequence 25 parallel: ")
    time(fibParallel(25))

    println("Fibonacci sequence 30 without parallel: ")
    time(fib(30))
    println("Fibonacci sequence 30 parallel: ")
    time(fibParallel(30))

    println("Fibonacci sequence 35 without parallel: ")
    time(fib(35))
    println("Fibonacci sequence 35 parallel: ")
    time(fibParallel(35))

    println("Fibonacci sequence 40 without parallel: ")
    time(fib(40))
    println("Fibonacci sequence 40 parallel: ")
    time(fibParallel(40))


  }


}

//OUTPUT FIBBONACI

//Fibonacci sequence 20 without parallel:
//Time equals: 10 ms
//Fibonacci sequence 20 parallel:
//Time equals: 73 ms
//Fibonacci sequence 25 without parallel:
//Time equals: 2 ms
//Fibonacci sequence 25 parallel:
//Time equals: 2 ms
//Fibonacci sequence 30 without parallel:
//Time equals: 25 ms
//Fibonacci sequence 30 parallel:
//Time equals: 15 ms
//Fibonacci sequence 35 without parallel:
//Time equals: 297 ms
//Fibonacci sequence 35 parallel:
//Time equals: 171 ms
//Fibonacci sequence 40 without parallel:
//Time equals: 3208 ms
//Fibonacci sequence 40 parallel:
//Time equals: 1991 ms
