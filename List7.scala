import scala.annotation.tailrec
import scala.concurrent.*
import ExecutionContext.Implicits.global
import duration.*
import scala.util.Random

object List7 {

  // task 1:
  // For given List[List[Int]] find the biggest number

  // conclusion:
  // List.max operation is too short, so creating threads takes
  // more time than non parallel approach


  //non parallel
  def t1N(list: List[List[Int]]): Int = {
    @tailrec
    def iterator(l: List[List[Int]], max: Int): Int =
      l match
        case h::t => iterator(t, {/*Thread.sleep(1000);*/ math.max(h.max, max)})
        case _ => max

    iterator(list, Int.MinValue)
  }
  //parallel
  def t1P(list: List[List[Int]]): Int = {
    val futures = for (x <- list) yield Future{/*Thread.sleep(1000);*/x.max}
    val ints = futures.map(Await.result(_, Duration.Inf))
    ints.max
  }

  def t1Test(n: Int, s: Int): List[List[Int]] = {
    @tailrec
    def iterator(k: Int, result: List[List[Int]]): List[List[Int]] =
      k match
        case 0 => result
        case _ => iterator(k - 1, generateList(s, List())::result)

    @tailrec
    def generateList(k: Int, result: List[Int]): List[Int] =
      k match
        case 0 => result
        case _ => generateList(k - 1, Random.nextInt()::result)

    iterator(n, List())
  }

  // task 2:
  // implement mergesort
  // conclusion: for 50mil elements in array parallel sort is 3x times faster than seqential.
  // But in my implementation if array.lenght = 100mil parallel sort run into OutOfMemoryError
  // whereas sequential one doesn't

  //non parallel:
  def t2N(arr: Array[Int]): Array[Int] = {
    def mergesort(l: Int, r: Int): Array[Int] =
      if r == l then Array(arr(r))
      else merge(mergesort(l, (r + l)/2), mergesort((r + l)/2 + 1, r))

    mergesort(0, arr.length - 1)
  }

  //parallel:
  def t2P(arr: Array[Int]): Array[Int] ={
    def mergesort(l: Int, r: Int): Array[Int] =
      if r == l then Array(arr(r))
      else
        merge(mergesort(l, (r + l)/2), mergesort((r + l)/2 + 1, r))

    val n = arr.length - 1
    if n < 10000 then
      mergesort(0, n)
    else
      val f1 = Future{mergesort(0, n/4)}
      val f2 = Future{mergesort(n/4+1,n/2)}
      val f3 = Future{mergesort(n/2+1,3*n/4)}
      val f4 = Future{mergesort(3*n/4+1,n)}
      merge(merge(
        Await.result(f1, Duration.Inf), Await.result(f2, Duration.Inf)
      ), merge(
        Await.result(f3, Duration.Inf), Await.result(f4, Duration.Inf)
      ))
      /*val f1 = Future{mergesort(0, n/2)}
      val f2 = Future{mergesort(n/2+1, n)}
      merge(Await.result(f1, Duration.Inf), Await.result(f2, Duration.Inf))
      */
  }

  def merge(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {
    var i = 0
    var j = 0
    var k = 0
    val array = new Array[Int](arr1.length + arr2.length)

    while (i < arr1.length && j < arr2.length)
      if arr1(i) < arr2(j) then
        array(k) = arr1(i)
        i = i + 1
      else
        array(k) = arr2(j)
        j = j + 1
      k = k + 1

    while (i < arr1.length)
      array(k) = arr1(i)
      i = i + 1
      k = k + 1

    while (j < arr2.length)
      array(k) = arr2(j)
      j = j + 1
      k = k + 1

    return array
  }

  def generateArray(n : Int): Array[Int] =
    val array = new Array[Int](n)
    for(i <- 0 to n - 1)
      array(i) = Random.nextInt()
    return array

  def main(args: Array[String]): Unit = {
    var now = System.nanoTime()
    var timeElapsed = System.nanoTime()

    println("t1 test")
    val t1 = t1Test(10, 100000)
    now = System.nanoTime()
    t1N(t1)
    timeElapsed = System.nanoTime()
    println("non parallel: " + ((timeElapsed - now)/1000000).toString + "ms")

    now = System.nanoTime()
    t1P(t1)
    timeElapsed = System.nanoTime()
    println("parallel: " + ((timeElapsed - now)/1000000).toString + "ms")

    println("t2 test")
    val t2 = generateArray(50000000)
    now = System.nanoTime()
    t2N(t2)
    timeElapsed = System.nanoTime()
    println("non parallel: " + ((timeElapsed - now)/1000000).toString + "ms")

    now = System.nanoTime()
    t2P(t2)
    timeElapsed = System.nanoTime()
    println("parallel: " + ((timeElapsed - now)/1000000).toString + "ms")
  }
}
