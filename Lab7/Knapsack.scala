package Lab7

import scala.math.max
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


object Knapsack {

  def knapsack(availableSpace: Int, weight: Array[Int], value: Array[Int], numberOfElem: Int): Int = {

    if (numberOfElem == 0 || availableSpace == 0)
      return 0
    if (weight(numberOfElem - 1) > availableSpace) {
      knapsack(availableSpace, weight, value, numberOfElem - 1)
    } else {
      val element1 = value(numberOfElem - 1) + knapsack(availableSpace - weight(numberOfElem - 1), weight, value, numberOfElem - 1)
      val element2 = knapsack(availableSpace, weight, value, numberOfElem - 1)

      max(element1, element2);
    }
  }

  def knapsackFuture(availableSpace: Int, weight: Array[Int], value: Array[Int], numberOfElem: Int): Int = {

    if (numberOfElem == 0 || availableSpace == 0)
      return 0
    if (weight(numberOfElem - 1) > availableSpace) {
      knapsack(availableSpace, weight, value, numberOfElem - 1)
    } else {
      val fut1 = Future(knapsack(value(numberOfElem - 1) + availableSpace - weight(numberOfElem - 1), weight, value, numberOfElem - 1))
      val fut2 = Future(knapsack(availableSpace, weight, value, numberOfElem - 1))

      val el1 = Await.result(fut1, Duration.Inf)
      val el2 = Await.result(fut2, Duration.Inf)

      if el1 > el2 then el1
      else el2
    }
  }

}
