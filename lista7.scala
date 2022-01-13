import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object lista7 {

  def monteCarloPiSequential(repetitions: Long): Double =
    var count = 0.0
    var i = 0L
    while i < repetitions do
      val x = Random.nextDouble()
      val y = Random.nextDouble()
      if x * x + y * y <= 1 then count += 1
      i += 1

    (count / repetitions) * 4

  def monteCarloPiParallel(repetitions: Long, futuresAmount: Int): Double =
    val futures = List.fill(futuresAmount)(Future {
      val localRepetitions = repetitions / futuresAmount
      var i = 0L
      var count = 0.0
      while i < localRepetitions do
        val x = ThreadLocalRandom.current().nextDouble()
        val y = ThreadLocalRandom.current().nextDouble()
        if x * x + y * y <= 1 then count += 1
        i += 1

      (count / repetitions) * 4
    })
    val piValues = Await.result(Future.sequence(futures), Duration.Inf)
    piValues.sum


  def getMinor(matrix: Array[Array[Int]], column: Int): Array[Array[Int]] =
    matrix.drop(1).map(row => row.take(column) ++ row.drop(column + 1))



  def calculateDeterminantSequential(matrix: Array[Array[Int]]): Int =
    matrix.length match
      case 1 => matrix(0)(0)
      case 2 => matrix(0)(0) * matrix(1)(1) - matrix(0)(1) * matrix(1)(0)
      case _ =>
        var sum = 0
        for(i <- 0 until matrix.length)
          sum += (if i % 2 == 0 then 1 else - 1) * matrix(0)(i) * calculateDeterminantSequential(getMinor(matrix, i))
        sum

  def calculateDeterminantParallel(matrix : Array[Array[Int]]): Int =
    if matrix.length < 5 then
      calculateDeterminantSequential(matrix)
    else
      var sum = 0
      for(i <- 0 until matrix.length / 2)
        sum += (if i % 2 == 0 then 1 else - 1) * matrix(0)(i) * calculateDeterminantSequential(getMinor(matrix, i))

      val future = Future {
        var sum = 0
        for(i <- (matrix.length / 2) until matrix.length)
          sum += (if i % 2 == 0 then 1 else - 1) * matrix(0)(i) * calculateDeterminantSequential(getMinor(matrix, i))
        sum
      }
      Await.result(future, Duration.Inf) + sum


  def time[A](block: => A): (A, Long) =
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, t1 - t0)

  def main(args: Array[String]): Unit = {
    println(time {
      monteCarloPiSequential(100000000)
    })
    println(time {
      monteCarloPiParallel(100000000, 4)
    })
    val matrix = Array.tabulate(10, 10)((_, _)=>Random.nextInt)
    println(time {
      calculateDeterminantSequential(matrix)
    })
    println(time {
      calculateDeterminantParallel(matrix)
    })
  }
}
