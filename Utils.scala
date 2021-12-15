package main.paradygmaty

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration.*

object Utils {

  def printTime[A](block: => A, name: String): A =
    val startTime = System.nanoTime()
    val result = block
    val endTime = System.nanoTime()
    println(s"$name, Time: ${(endTime-startTime)} ns")
    result

  def printTimeMs[A](block: => A, name: String): A =
    val startTime = System.currentTimeMillis()
    val result = block
    val endTime = System.currentTimeMillis()
    println(s"$name, Time: ${endTime-startTime} ms")
    result

  def parallel[A, B](taskA: Future[A], taskB: Future[B]): (A, B) =
    val result =
      for
        r1 <- taskA
        r2 <- taskB
      yield
        (r1, r2)

    Await.result(result, Duration.Inf)


}

