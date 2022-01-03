import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Factorial {

  def countFatorial(number: BigInt, min: BigInt): BigInt = {
      if number == min then min
      else number * countFatorial(number - 1, min)
  }
  
  def countFactorialFuture(number: BigInt): BigInt = {
    val stepSize = (number / 5) + 1

    val f1 = Future {countFatorial(stepSize, 1)}
    val f2 = Future {countFatorial(stepSize * 2, stepSize + 1)}
    val f3 = Future {countFatorial(stepSize * 3, stepSize * 2 + 1)}
    val f4 = Future {countFatorial(number, stepSize * 3 + 1)}

    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
      r4 <- f4
    } yield (r1 * r2 * r3 * r4)
    Await.result(result, Duration.Inf)
  }

  def countFatorialParallel(number: BigInt, min: BigInt): BigInt = {
    val stepSize = (number / 5) + 1
    val (l1, l2, l3, l4) = parallel(countFatorial(stepSize, 1), countFatorial(stepSize * 2, stepSize + 1), countFatorial(stepSize * 3, stepSize * 2 + 1), countFatorial(number, stepSize * 3 + 1))
    l1 * l2 * l3 * l4
  }
  
  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A,B, C, D) = {
    val futureB: Future[B] = Future { taskB }
    val futureC: Future[C] = Future { taskC }
    val futureD: Future[D] = Future { taskD }
    val a: A = taskA
    val b: B = Await.result(futureB, Duration.Inf)
    val c: C = Await.result(futureC, Duration.Inf)
    val d: D = Await.result(futureD, Duration.Inf)
    (a, b, c, d)
  }
}
