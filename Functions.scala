import scala.concurrent.*
import ExecutionContext.Implicits.global
import duration.*
import scala.annotation.tailrec

object Functions {

  def parallel[A, B](taskA: => A, taskB: => B):(A,B) = {
    val right = Future{taskA}
    val left = Future{taskB}
    (Await.result(right, Duration.Inf), Await.result(left, Duration.Inf))
  }

  def sumOfPair(pair: (Double, Double)): Double = pair._1 + pair._2

  def split[A](list: List[A]): (List[A], List[A]) =
    @tailrec
    def inner(length: Int, first: List[A], second: List[A]): (List[A], List[A]) =
      (second, length) match
        case(_, 0) => (first, second)
        case(h::t, _) => inner(length - 1, h::first, t)
    inner(list.length/2, List(), list)

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  def testTime[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    operation
    operation
    operation
    operation
    (System.currentTimeMillis() - time)/5
  }
}