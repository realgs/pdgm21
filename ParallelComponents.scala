import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object ParallelComponents
{
  def measureTimeWithResult[A](toMeasure: => A): (A, Long) =
    val timeStart = System.nanoTime()
    val runCode = toMeasure
    val timeEnd = System.nanoTime()
    (runCode, timeEnd - timeStart)

  def measureTime[A](toMeasure: => A): Long =
    val timeStart = System.nanoTime()
    toMeasure
    val timeEnd = System.nanoTime()
    timeEnd - timeStart

  def parallel[A, B](taskA: Future[A], taskB: Future[B]): (A, B) =
    val result =
      for
        r1 <- taskA
        r2 <- taskB
      yield
        (r1, r2)
    Await.result(result, Duration.Inf)

  def parallelName[A, B](taskA: => A, taskB: => B): (A, B) =
    val right: Future[B] = Future {
      taskB
    }
    val left: A = taskA
    (left, Await.result(right, Duration.Inf))
}
