//Szymon Sawczuk

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

def time[T](task: => T): Long =
  val t0 = System.nanoTime()
  val result = task
  val t1 = System.nanoTime()
  val time = (t1 - t0)
//  println(s"time: $time ns")
  time


object Lista7:
  def main(args: Array[String]): Unit =
    val numberOfCores = Runtime.getRuntime.availableProcessors()
    var time1: Long= 0
    var time2: Long = 0
    var time3: Long = 0
    val numberOfTries = 100

    println("Number of cores: " + numberOfCores)

//  SumOf2DArray
//  500 elem
    var arr0 = SumOf2DArray.generate2DArray(500, 500)
    for i <- 0 to numberOfTries - 1 do
      time1 += time({SumOf2DArray.sumOf2DArray(arr0)})
      time2 += time({SumOf2DArray.sumOf2DArrayPar(arr0, numberOfCores)})
    println(s"Time of not parallel method(SumOf2DArray 500 x 500 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(SumOf2DArray 500 x 500  elem): ${ (time2/numberOfTries) * 1e-9 }s\n")

//  1000 elem
    arr0 = SumOf2DArray.generate2DArray(1000, 1000)
    for i <- 0 to numberOfTries - 1 do
      time1 += time({SumOf2DArray.sumOf2DArray(arr0)})
      time2 += time({SumOf2DArray.sumOf2DArrayPar(arr0, numberOfCores)})
    println(s"Time of not parallel method(SumOf2DArray 1 000 x 1 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(SumOf2DArray 1 000 x 1 000  elem): ${ (time2/numberOfTries) * 1e-9 }s\n")

//  5000 elem
    arr0 = SumOf2DArray.generate2DArray(5000, 5000)
    for i <- 0 to numberOfTries - 1 do
      time1 += time({SumOf2DArray.sumOf2DArray(arr0)})
      time2 += time({SumOf2DArray.sumOf2DArrayPar(arr0, numberOfCores)})
    println(s"Time of not parallel method(SumOf2DArray 5 000 x 5 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(SumOf2DArray 5 000 x 5 000 elem): ${ (time2/numberOfTries) * 1e-9 }s\n")

//  10000 elem
    arr0 = SumOf2DArray.generate2DArray(10000, 10000)
    for i <- 0 to numberOfTries - 1 do
      time1 += time({SumOf2DArray.sumOf2DArray(arr0)})
      time2 += time({SumOf2DArray.sumOf2DArrayPar(arr0, numberOfCores)})
    println(s"Time of not parallel method(SumOf2DArray 10 000 x 10 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(SumOf2DArray 10 000 x 10 000 elem): ${ (time2/numberOfTries) * 1e-9 }s\n")

//  20000 elem
    arr0 = SumOf2DArray.generate2DArray(10000, 10000)
    for i <- 0 to numberOfTries - 1 do
      time1 += time({SumOf2DArray.sumOf2DArray(arr0)})
      time2 += time({SumOf2DArray.sumOf2DArrayPar(arr0, numberOfCores)})
    println(s"Time of not parallel method(SumOf2DArray 20 000 x 20 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(SumOf2DArray 20 000 x 20 000 elem): ${ (time2/numberOfTries) * 1e-9 }s\n")

//  QuickSort
//  100 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 100 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 100 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 100 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 100 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")

//  1000 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 1000 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 1 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 1 000 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 1 000 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")

//  5000 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 5000 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 5 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 5 000 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 5 000 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")


//  10 000 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 10000 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 10 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 10 000 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 10 000 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")

//  100 000 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 100000 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 100 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 100 000 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 100 000 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")

//  1 000 000 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 1000000 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 1 000 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 1 000 000 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 1 000 000 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")

//  10 000 000 elem
    time1 = 0
    time2 = 0
    for i <- 0 to numberOfTries - 1 do
      val arr1: Array[Int] = (for(i <- 0 to 10000000 - 1) yield scala.util.Random.nextInt(10000)).toArray
      val arr2: Array[Int] = arr1.clone()
      val arr3: Array[Int] = arr1.clone()
      time1 += time({QuickSort.quickSort(arr1)})
      time2 += time({QuickSort.quickSortPar(arr2, numberOfCores)})
      time3 += time({QuickSort.quickSortParMixed(arr3)})

    println(s"Time of not parallel method(QuickSort 10 000 000 elem): ${ (time1/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method(QuickSort 10 000 000 elem): ${ (time2/numberOfTries) * 1e-9 }s")
    println(s"Time of parallel method only first partition(QuickSort 10 000 000 elem): ${ (time3/numberOfTries) * 1e-9 }s\n")
