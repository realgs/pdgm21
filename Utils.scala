import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Utils {

  def time[A](functionToMeasure: => A) =
    val startTime = System.nanoTime()
    functionToMeasure
    System.nanoTime() - startTime


  def parallel[A, B](taskA: => A, taskB: => B): (A,B) =
    val right = Future{taskB}
    val left = taskA
    (left, Await.result(right, Duration.Inf))
}
