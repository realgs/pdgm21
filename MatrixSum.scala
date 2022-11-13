import scala.concurrent.{Future, Await}
import scala.util.Random
import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object MatrixSum {

  def printMatrix[A](matrix: List[List[A]]) =
    matrix.map(_.mkString(" ")).foreach(println)

  def sumMatrixElements(matrix: List[List[Int]]): Int =
    matrix.foldLeft(0)((sumOfElements, x) =>
      sumOfElements + x.foldLeft(0)((sumOfElements, y) => sumOfElements + y))

  def sumLine(line: List[Int]): Int =
    line.foldLeft(0)(_ + _)

  def SumMatrixElementsFuture(matrix: List[List[Int]]): Int=
    val stepSize = (matrix.size / 6) + 1

    val f1 = Future { sumMatrixElements(matrix.slice(0, stepSize))}
    val f2 = Future { sumMatrixElements(matrix.slice(stepSize, 2*stepSize))}
    val f3 = Future { sumMatrixElements(matrix.slice(2*stepSize, 3*stepSize))}
    val f4 = Future { sumMatrixElements(matrix.slice(3*stepSize, 4*stepSize+1))}
    val f5 = Future { sumMatrixElements(matrix.slice(4*stepSize, 5*stepSize+1))}

    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
      r4 <- f4
      r5 <- f5
    } yield (r1 :: r2 :: r3 :: r4 :: r5 :: Nil)
    sumLine(Await.result(result, Duration.Inf))

  def sumMatrixElementsParallel(matrix: List[List[Int]]): Int = {
    val stepSize = (matrix.size / 5) + 1
    val (l1, l2, l3, l4, l5) = parallel(sumMatrixElements(matrix.slice(0, stepSize)), sumMatrixElements(matrix.slice(stepSize, 2*stepSize)), sumMatrixElements(matrix.slice(2*stepSize, 3*stepSize)), sumMatrixElements(matrix.slice(3*stepSize, 4*stepSize+1)), sumMatrixElements(matrix.slice(4*stepSize, 5*stepSize+1)))
    (l1 + l2 + l3 + l4 + l5)
  }

  def parallel[A, B, C, D, E](taskA: => A, taskB: => B, taskC: => C, taskD: => D, taskE: => E): (A,B, C, D, E) = {
    val futureB: Future[B] = Future { taskB }
    val futureC: Future[C] = Future { taskC }
    val futureD: Future[D] = Future { taskD }
    val futureE: Future[E] = Future { taskE }
    val a: A = taskA
    val b: B = Await.result(futureB, Duration.Inf)
    val c: C = Await.result(futureC, Duration.Inf)
    val d: D = Await.result(futureD, Duration.Inf)
    val e: E = Await.result(futureE, Duration.Inf)
    (a, b, c, d, e)
  }
}
