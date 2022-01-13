package io.github.reconsolidated

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source

object Main {

  def sqrtArray(from: Integer, to: Integer, arg: ArrayBuffer[Double]): Unit = {
    val range = Range(from, to)
    for (n <- range) {
      arg(n) = math.sqrt(arg(n))
    }
  }


  def sumOfSqrtArray(from: Integer, to: Integer, arg: ArrayBuffer[Double]): Double = {
    val range = Range(from, to)
    var result = 0.0
    for (n <- range) {
      result += math.sqrt(arg(n))
    }
    result
  }

  def getRandomList(n: Integer): ArrayBuffer[Double] = {
    val result = ArrayBuffer[Double]()
    val range = 1 to n
    val random = new scala.util.Random
    for (n <- range) {
      result += random.nextInt(100).toDouble
    }
    result
  }


  def lookForStringInFile(fileName: String, needle: String): Boolean = {
    var source = Source.fromFile(fileName)
    var lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    source = Source.fromFile(fileName)
    lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    source = Source.fromFile(fileName)
    lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    source = Source.fromFile(fileName)
    lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    source = Source.fromFile(fileName)
    lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    source = Source.fromFile(fileName)
    lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    source = Source.fromFile(fileName)
    lines = try scala.io.Source.fromFile(fileName).mkString finally source.close()

    lines.contains(needle)
  }



  def main(args: Array[String]): Unit = {
    println("// Finding string in file //")

    val name = "Tadeuszewy"
    var startTime = System.currentTimeMillis()

    var s1 = Future[Boolean]{lookForStringInFile("test1.txt", name)}
    var s2 = Future[Boolean]{lookForStringInFile("test2.txt", name)}
    var s3 = Future[Boolean]{lookForStringInFile("test3.txt", name)}
    var s4 = Future[Boolean]{lookForStringInFile("test4.txt", name)}
    var r1 = Await.result(s1, Duration.Inf)
    var r2 = Await.result(s2, Duration.Inf)
    var r3 = Await.result(s3, Duration.Inf)
    var r4 = Await.result(s4, Duration.Inf)
    var difference = System.currentTimeMillis() - startTime
    println("Word in text: " + (r1 || r2 || r3 || r4))
    println("Asynchronous: " + difference)

    startTime = System.currentTimeMillis()
    println("Word in text: " + (lookForStringInFile("test1.txt", name) ||
    lookForStringInFile("test2.txt", name) ||
    lookForStringInFile("test3.txt", name) ||
    lookForStringInFile("test4.txt", name)))
    difference = System.currentTimeMillis() - startTime
    println("Synchronous time: " + difference)
    println("")



    println("// Sqrt of array //")

    val size = 50000000
    val list = getRandomList(size)
    var clone = list.clone()
    startTime = System.currentTimeMillis()
    sqrtArray(0, size, clone)
    difference = System.currentTimeMillis() - startTime

    println("Synchronous Time: " + difference)

    clone = list.clone()
    startTime = System.currentTimeMillis()
    var f1 = Future{sqrtArray(0, size/4, clone)}
    var f2 = Future{sqrtArray(size/4, size/2, clone)}
    var f3 = Future{sqrtArray(size/2, size*3/4, clone)}
    var f4 = Future{sqrtArray(size * 3 / 4, size, clone)}
    Await.result(f1, Duration.Inf)
    Await.result(f2, Duration.Inf)
    Await.result(f3, Duration.Inf)
    Await.result(f4, Duration.Inf)
    difference = System.currentTimeMillis() - startTime
    println("Asynchronous Time: " + difference)


    startTime = System.currentTimeMillis()
    var result = sumOfSqrtArray(0, size, clone)
    println(result)
    difference = System.currentTimeMillis() - startTime
    println("Synchronous Time: " + difference)


    println("// Sum of sqrt array //")

    startTime = System.currentTimeMillis()
    result = 0.0
    var fd1 = Future[Double]{sumOfSqrtArray(0, size/4, clone)}
    var fd2 = Future[Double]{sumOfSqrtArray(size/4, size/2, clone)}
    var fd3 = Future[Double]{sumOfSqrtArray(size/2, size*3/4, clone)}
    var fd4 = Future[Double]{sumOfSqrtArray(size * 3 / 4, size, clone)}
    result += Await.result(fd1, Duration.Inf)
    result += Await.result(fd2, Duration.Inf)
    result += Await.result(fd3, Duration.Inf)
    result += Await.result(fd4, Duration.Inf)
    difference = System.currentTimeMillis() - startTime
    println(result)
    println("Asynchronous Time: " + difference)




  }


}
