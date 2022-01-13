//Szymon Sawczuk

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

object SumOf2DArray:
  def generate2DArray(rows: Int, columns: Int): Array[Array[Int]]=
    val random = scala.util.Random
    var resultArr = Array.ofDim[Int](rows, columns)

    for i <- 0 to rows - 1 do
      for j <- 0 to columns - 1 do
        resultArr(i)(j) = random.nextInt()

    resultArr

  def sumOf2DArray(arr: Array[Array[Int]]): Int =
    var result = 0
    for (i <- 0 to arr.length - 1) do
      for (j <- 0 to arr(i).length - 1) do
        result += arr(i)(j)
    result

  def sum(arr: Array[Array[Int]], rows: Int, columns: Int, endRows: Int): Int =
    var result = 0
    for i <- rows to endRows - 1 do
      for j <- 0 to columns - 1 do
        result += arr(i)(j)
    result

  def sumOf2DArrayPar(arr: Array[Array[Int]], cores: Int): Int =
    var result = 0
    val oneDivison: Int = arr.length / cores
    var currentIndex: Int = 0 - oneDivison
    val columns = arr(0).length

    val indexes = Seq.fill(cores)({currentIndex += oneDivison; currentIndex})
    val futures = for (i <- indexes) yield Future{sum(arr, i, columns, i + oneDivison)}
    val results = futures.map(Await.result(_, Duration.Inf))

    results.foreach(elem => result += elem)
    result

