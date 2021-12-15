package main.paradygmaty

import main.paradygmaty.Utils.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import java.util.concurrent.ForkJoinTask.invokeAll
import java.util.concurrent.{ForkJoinPool, RecursiveTask}
import scala.concurrent.duration._

object Fibonacci {

  def fibonacciNormal(num: Int): BigInt =
    if num < 0 then throw new Exception("index number can not be nagative number!")
    else
      num match
        case 0 => 0
        case 1 => 1
        case _ => fibonacciNormal(num - 1) + fibonacciNormal(num - 2)


  def fibonacciTailrec(num: Int): BigInt =
    @tailrec
    def fibTailHelper(a: BigInt, b: BigInt, i: Int = 0): BigInt =
      if i < num then
        fibTailHelper(b, a + b, i + 1)
      else
        a

    fibTailHelper(0, 1)


  def fibLazy(num: Int): BigInt =
    def lazyFib(x: BigInt = 1, y: BigInt = 1): LazyList[BigInt] =
      x #:: lazyFib(y, x+y)

    if num > 0 then lazyFib().take(num).last
    else if num == 0 then 0 // can't take(0) and then last from empty lazy list
    else throw new Exception("awdad")

  
  def fibonacciDoubleTailParallel(num: Int): BigInt =
    def fibonacciDTPHelper(num: Int): BigInt =
      val result = Utils.parallel(Future(fibonacciTailrec(num-1)), Future(fibonacciTailrec(num-2)))
      (result._1 + result._2)
    if num < 0 then throw new Exception("index number can not be nagative number!")
    else fibonacciDTPHelper(num)


  // minNum = 10 - 15 is most effective in my experience, discovered during testing

  def fibonacciParallelFork(num: Int, minNum: Int = 15): BigInt =
    if num < 0 then throw new Exception("index number can not be nagative number!")
    else new ForkJoinPool().invoke(new FibonacciParallel(num, minNum))


  class FibonacciParallel(num: Int, private val minNum: Int = 15) extends RecursiveTask[BigInt]{

    override def compute(): BigInt =
      num match
        case 0 => 0
        case 1 => 1
        case _ =>
            if num <= minNum then
              fibonacciNormal(num - 1) + fibonacciNormal(num - 2)
            else
              val left = new FibonacciParallel(num-1)
              val right = new FibonacciParallel(num-2)
              invokeAll(left, right)

              left.get() + right.get()

    //no exception needed here
    private def fibonacciNormal(num: Int): BigInt =
        num match
          case 0 => 0
          case 1 => 1
          case _ => fibonacciNormal(num - 1) + fibonacciNormal(num - 2)



  }



  /*
  //minNum było odpowiednikiem głębokości na jaką miało się dzielić
  //zapobiegało to nieskończonemu tworzeniu się wątków i podziałowi
    Lepiej użyć dual tailrec, troche wolniejsze jednak dużo bardziej
    odporne na stackoverflow
  */

  def fibonacciForkTail(num: Int, minNum: Int = 1000): BigInt =
    if num < 0 then throw new Exception("index number can not be nagative number!")
    else new ForkJoinPool().invoke(new FibonacciTailParallelFork(num, minNum))

  class FibonacciTailParallelFork(num: Int, private val minNum: Int = 1000) extends RecursiveTask[BigInt] {
    override def compute(): BigInt =
      num match
        case 0 => 0
        case 1 => 1
        case _ =>
          if num <= minNum then
            fibonacciTailrec(num)
          else
            val left = new FibonacciTailParallelFork(num-1)
            val right = new FibonacciTailParallelFork(num-1)
            invokeAll(left, right)

            left.get() + right.get()

  }

/*
  //minNum było odpowiednikiem głębokości na jaką miało się dzielić
  //zapobiegało to nieskończonemu tworzeniu się wątków i podziałowi
  def fibonacciParallel(num: Int): BigInt = {

    def fibonacciParallelHelper(num: Int, minNum: Int): BigInt =
      if minNum <= 0 then fibonacciNormal(num)
      else
        num match
          case 0 => {
            0
          }
          case 1 => 1
          case _ =>
            val result =
              for
                r1 <- Future(fibonacciParallelHelper(num-1, minNum - 1))
                r2 <- Future(fibonacciParallelHelper(num-2, minNum - 2))
              yield
                (r1 + r2)
            Await.result(result, Duration.Inf)

    if num < 0 then throw new Exception("index number can not be nagative number!")
    else fibonacciParallelHelper(num, 8)
  //fibonacciParallelHelper(num-1, 8)
  }

*/

}

