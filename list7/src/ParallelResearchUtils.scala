import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object ParallelResearchUtils {
  def parallel[A, B](taskA: => A, taskB: => B): (A,B) = {
    val future: Future[B] = Future {
      taskB
    }
    val a: A = taskA
    val b: B = Await.result(future, Duration.Inf)
    (a, b)
  }
  def timer[A](task: => A): Long ={
    val start = System.currentTimeMillis()
    task
    System.currentTimeMillis() - start
  }
}
