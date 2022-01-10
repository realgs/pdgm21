import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import Utils.{time, parallel}

object AbsOfElementsInArray {

  def absArray(array: Array[Int], startIndex: Int, endIndex: Int): Array[Int] =
    if (startIndex < 0) || (endIndex > array.length - 1) || (endIndex < startIndex) then array
    else
      for (i <- startIndex to endIndex)
        if array(i) < 0 then array(i) = array(i) * (-1)
      array


  def absArrayFuture(array: Array[Int], startIndex: Int, endIndex: Int): Array[Int] =
    if (startIndex < 0) || (endIndex > array.length - 1) || (endIndex < startIndex) then array
    else
      val middleIndex = startIndex + ((endIndex - startIndex + 1) / 2)
      val left = Future{absArray(array, startIndex, middleIndex - 1)}
      val right = Future{absArray(array, middleIndex, endIndex)}
      Await.result(left, Duration.Inf)
      Await.result(right, Duration.Inf)
      array


  def absArrayParallel(array: Array[Int], startIndex: Int, endIndex: Int): Array[Int] =
    if (startIndex < 0) || (endIndex > array.length - 1) || (endIndex < startIndex) then array
    else
      val middleIndex = startIndex + ((endIndex - startIndex + 1) / 2)
      parallel(absArray(array, startIndex, middleIndex - 1), absArray(array, middleIndex, endIndex))
      array


  def main(args: Array[String]): Unit = {
    println("Testy wartosci bezwzglednej elementow w tablicy:")
    println()

    println("Testy poprawnosci:")
    val r = scala.util.Random
    var array = Array.fill(10)(r.nextInt(200) - 100)
    var arrayCopy1 = array.clone()
    var arrayCopy2 = array.clone()

    println("input array: " + array.toList)
    println("normal:      " + absArray(array, 0, array.length - 1).toList)
    println("future:      " + absArrayFuture(arrayCopy1, 0, array.length - 1).toList)
    println("parallel:    " + absArrayParallel(arrayCopy2, 0, array.length - 1).toList)
    println()

    array = Array.fill(10)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("input array: " + array.toList)
    println("normal:      " + absArray(array, 5, array.length - 1).toList)
    println("future:      " + absArrayFuture(arrayCopy1, 5, array.length - 1).toList)
    println("parallel:    " + absArrayParallel(arrayCopy2, 5, array.length - 1).toList)
    println()

    array = Array.fill(10)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("input array: " + array.toList)
    println("normal:      " + absArray(array, 9, array.length - 1).toList)
    println("future:      " + absArrayFuture(arrayCopy1, 9, array.length - 1).toList)
    println("parallel:    " + absArrayParallel(arrayCopy2, 9, array.length - 1).toList)
    println()


    println("Testy przy zmiennej dlugosci tablicy:")
    array = Array.fill(10)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 10")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(100)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 100")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(500)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 500")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(1000)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 1000")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(10000)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 10000")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(100000)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 100000")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(1000000)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 1000000")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()

    array = Array.fill(10000000)(r.nextInt(200) - 100)
    arrayCopy1 = array.clone()
    arrayCopy2 = array.clone()
    println("length: 10000000")
    println("normal:   " + time(absArray(array, 0, array.length - 1)))
    println("future:   " + time(absArrayFuture(arrayCopy1, 0, array.length - 1)))
    println("parallel: " + time(absArrayParallel(arrayCopy2, 0, array.length - 1)))
    println()
  }
}
