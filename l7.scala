import sun.security.util.Length

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object l7 {

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val f1 = Future{ taskA }
    val f2 = Future{ taskB }
    val res1 = Await.result(f1, Duration.Inf)
    val res2 = Await.result(f2, Duration.Inf)
    (res1, res2)

  def getRandomArray(length: Int): Array[Int] =
    val array: Array[Int] = new Array(length);
    for (i <- 0 until length)
      array(i) = Random.nextInt(10000)
    array

  def powerAndSum(array: Array[Int], power: Int, firstIdx: Int, lastIdx: Int): Int =
    var sum: Int = 0;
    for (i <- firstIdx until lastIdx)
      sum = sum + math.pow(array(i), power).toInt
    sum

  val threshold = 200000
  def powerAndSumParallel(array: Array[Int], power: Int, firstIdx: Int, lastIdx: Int): Int =
    if (lastIdx - firstIdx < threshold) then powerAndSum(array, power, firstIdx, lastIdx)
    else
      val mid = firstIdx + (lastIdx - firstIdx) / 2
      val (sum1, sum2) = parallel(powerAndSumParallel(array, power, firstIdx, mid), powerAndSumParallel(array, power, mid, lastIdx))
      sum1 + sum2

  def circleHits(attempts: Int): Int =
    var hits: Int = 0
    val randomX = new Random
    val randomY = new Random
    for (i <-0 until attempts)
      val x = randomX.nextDouble
      val y = randomY.nextDouble
      if (x*x + y*y < 1) hits = hits + 1
    hits

  def piSeq(hits: Int): Double = 4.0 * circleHits(hits) / hits

  def piParallel(hits: Int): Double =
    val ((res1, res2), (res3, res4)) = parallel(
      parallel(circleHits(hits/4), circleHits(hits/4)),
      parallel(circleHits(hits/4), circleHits(hits/4))
    )
    4.0 * (res1 + res2 + res3 + res4) / hits

  def main(args: Array[String]): Unit = {
    /*
    val array = getRandomArray(500000);

    val s1 = System.currentTimeMillis()
    val sum = powerAndSum(array, 2, 0, array.length)
    val f1 = System.currentTimeMillis()
    println(s"Sequential: ${f1-s1}")

    val s2 = System.currentTimeMillis()
    val sumPar = powerAndSumParallel(array, 2, 0, array.length)
    val f2 = System.currentTimeMillis()
    println(s"Parallel: ${f2 - s2}")
    */

    val s1 = System.currentTimeMillis()
    val pi: Double = piSeq(1000000)
    val f1 = System.currentTimeMillis()
    println(s"Sequential: ${f1-s1}")

    val s2 = System.currentTimeMillis()
    val piPar: Double = piParallel(1000000)
    val f2 = System.currentTimeMillis()
    println(s"Parallel: ${f2 - s2}")

  }

}
