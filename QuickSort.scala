//Szymon Sawczuk

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

object QuickSort:
  val numberOfCores = Runtime.getRuntime.availableProcessors()

  def swap(arr:Array[Int], leftElement: Int, rightElement: Int):Unit =
    val temp = arr(leftElement)
    arr(leftElement) = arr(rightElement)
    arr(rightElement) = temp

  def makePartition(arr:Array[Int], left: Int, right: Int): Int =
    val partitionIndex = left + (right - left) / 2
    val partitionValue = arr(partitionIndex)
    swap(arr, partitionIndex, right)

    var actualPosition = left
    for i <- left to right - 1 do
      if arr(i) < partitionValue then
        swap(arr, i, actualPosition)
        actualPosition += 1

    swap(arr, actualPosition, right)
    actualPosition

  def quickSort(arr: Array[Int]): Unit =
    if arr.length <= 1 then ()
    else quickSort(arr, 0, arr.length - 1)

  def quickSort(arr:Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val actualPosition = makePartition(arr, left, right)

      quickSort(arr, left, actualPosition - 1)
      quickSort(arr, actualPosition + 1, right)

  def parallel[A, B](taskA: =>A, taskB: =>B):(A, B) =
    val f1: Future[A] = Future(taskA)
    val f2: Future[B] = Future(taskB)
    val result = (Await.result(f1, Duration.Inf), Await.result(f2, Duration.Inf))
    result

  def quickSortPar(arr: Array[Int], cores: Int): Unit =
    if arr.length <= 1 then ()
    else quickSortPar(arr, 0, arr.length - 1, cores)

  def quickSortPar(arr:Array[Int], left: Int, right: Int, cores: Int): Unit =
    if left < right then
      val actualPosition = makePartition(arr, left, right)
      if cores == -(numberOfCores * numberOfCores - 2) then
        quickSortPar(arr, left, actualPosition - 1, cores)
        quickSortPar(arr, actualPosition + 1, right, cores)
      else
        parallel(quickSortPar(arr, left, actualPosition - 1, cores - 2), quickSortPar(arr, actualPosition + 1, right, cores - 2))

  def quickSortParMixed(arr: Array[Int]): Unit =
    if arr.length <= 1 then ()
    else quickSortParMixed(arr, 0, arr.length - 1)

  def quickSortParMixed(arr:Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val actualPosition = makePartition(arr, left, right)
      parallel(quickSort(arr, left, actualPosition - 1), quickSort(arr, actualPosition + 1, right))
