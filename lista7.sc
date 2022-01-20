import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object lista7 {
  def randomWords(n: Int): List[String] =
    val r = new scala.util.Random

    def randomWord(n: Int): String =
      if n == 0 then
        ""
      else
        val c = (r.nextInt(26) + 'a').toChar
        c + randomWord(n - 1)

    (1 to n).map(i => randomWord(r.nextInt(10) + 1)).toList


  def generateRandomList(n: Int, seed: Int = 1): List[Int] = {
    val r = new scala.util.Random(seed)
    var list = List[Int]()
    while (list.length < n)
      list = r.nextInt(100) :: list
    list
  }

  def generateRandomArray(n: Int, seed: Int = 1): Array[Int] = {
    generateRandomList(n, seed).toArray
  }

  def printArray(array: Array[Int]): Unit = {
    array.foreach(i => print(i + " "))
    println()
  }

  def joinAndSort(list1: List[Int], list2: List[Int], list3: List[Int], list4: List[Int]): List[Int] =
    val list = list1 ::: list2 ::: list3 ::: list4
    list.sorted


  def testTime(function: => Unit, message: String): Long = {
    var time = System.nanoTime()
    function
    time = System.nanoTime() - time
    if (time < 1000000000)
      println(s"$message: ${time / 1000.0} us")
    else
      println(s"$message: ${time / 1000000.0} ms")
    time
  }

  def compare(function1: => Unit, function2: => Unit, dataSize: Int, function1Name: String, function2Name: String): Unit = {
    println(s"\n\nData size: $dataSize")
    val time1 = testTime(function1, function1Name)
    val time2 = testTime(function2, function2Name)
    if time1 < time2 then
      println(s"$function1Name is ${time2 * 100 / time1 - 100}% faster than $function2Name")
    else
      println(s"$function2Name is ${time1 * 100 / time2 - 100}% faster than $function1Name")
  }


  def parallel[A, B](a: => A, b: => B): (A, B) =
    val t1 = Future {
      a
    }
    val t2 = Future {
      b
    }
    (Await.result(t1, Duration.Inf), Await.result(t2, Duration.Inf))


  def normalFibonacci(num: Int): Long =
    require(num >= 0, "number can't be negative")
    if num < 2 then
      num
    else
      normalFibonacci(num - 1) + normalFibonacci(num - 2)


  def parallelFibonacci(num: Int): Long =
    require(num >= 0, "number can't be negative")

    def parallelFibonacciInternal(num: Int, depth: Int): Long =
      if num < 35 || depth <= 0 then //35 wynika z kilkukrotnych testów
        if num < 2 then
          num
        else
          normalFibonacci(num - 1) + normalFibonacci(num - 2)
      else
        val (fib1, fib2) = parallel(parallelFibonacciInternal(num - 1, depth - 1), parallelFibonacciInternal(num - 2, depth - 1))
        fib1 + fib2

    parallelFibonacciInternal(num, 5)


  def normalFindPrimes(n: Int): List[Int] = {
    require(n > 1, "number can't be less than 2")

    @tailrec
    def findPrimesRec(n: Int, primes: List[Int]): List[Int] =
      if n < 2 then
        primes
      else if isPrime(n) then
        findPrimesRec(n - 1, n :: primes)
      else
        findPrimesRec(n - 1, primes)

    findPrimesRec(n, List())
  }

  def isPrime(i: Int): Boolean =
    if i <= 1 then
      false
    else if i == 2 then
      true
    else
      !(2 until i).exists(n => i % n == 0)

  def parallelFindPrimes(n: Int): List[Int] =
    require(n > 1, "number can't be less than 2")
    if n < 100 then
      normalFindPrimes(n)
    else
      @tailrec
      def findPrimes(n: Int, primes: List[Int]): List[Int] =
        if (n < 2) primes
        else if (isPrime(n)) findPrimes(n - 4, n :: primes)
        else findPrimes(n - 4, primes)

      val a = Future {
        findPrimes(n, List())
      }
      val b = Future {
        findPrimes(n - 1, List())
      }
      val c = Future {
        findPrimes(n - 2, List())
      }
      val d = Future {
        findPrimes(n - 3, List())
      }
      joinAndSort(Await.result(a, Duration.Inf), Await.result(b, Duration.Inf), Await.result(c, Duration.Inf), Await.result(d, Duration.Inf))


  def main(args: Array[String]): Unit = {

    compare(normalFindPrimes(100), parallelFindPrimes(100), 100, "normalFindPrimes", "parallelFindPrimes")
    compare(normalFindPrimes(1000), parallelFindPrimes(1000), 1000, "normalFindPrimes", "parallelFindPrimes")
    compare(normalFindPrimes(10000), parallelFindPrimes(10000), 10000, "normalFindPrimes", "parallelFindPrimes")
    compare(normalFindPrimes(100000), parallelFindPrimes(100000), 100000, "normalFindPrimes", "parallelFindPrimes")

    /*
    *
    Data size: 100
    normalFindPrimes: 20888.7 us
    parallelFindPrimes: 67418.3 us
    normalFindPrimes is 222% faster than parallelFindPrimes


    Data size: 1000
    normalFindPrimes: 6003.7 us
    parallelFindPrimes: 5780.4 us
    parallelFindPrimes is 3% faster than normalFindPrimes


    Data size: 10000
    normalFindPrimes: 28331.5 us
    parallelFindPrimes: 20516.4 us
    parallelFindPrimes is 38% faster than normalFindPrimes


    Data size: 100000
    normalFindPrimes: 1794.9638 ms
    parallelFindPrimes: 1011.5418 ms
    parallelFindPrimes is 77% faster than normalFindPrimes
    * */



    compare(normalFibonacci(47), parallelFibonacci(47), 47, "normalFib", "parallelFib")
    compare(normalFibonacci(45), parallelFibonacci(45), 45, "normalFib", "parallelFib")
    compare(normalFibonacci(42), parallelFibonacci(42), 42, "normalFib", "parallelFib")
    compare(normalFibonacci(40), parallelFibonacci(40), 40, "normalFib", "parallelFib")
    compare(normalFibonacci(30), parallelFibonacci(30), 30, "normalFib", "parallelFib")
    compare(normalFibonacci(20), parallelFibonacci(20), 20, "normalFib", "parallelFib")
    compare(normalFibonacci(10), parallelFibonacci(10), 10, "normalFib", "parallelFib")

    /*
    * Data size: 45
    normalFib: 5146.9666 ms
    parallelFib: 2557.9517 ms
    parallelFib is 101% faster than normalFib


    Data size: 42
    normalFib: 1230.192 ms
    parallelFib: 273847.0 us
    parallelFib is 349% faster than normalFib


    Data size: 40
    normalFib: 470976.6 us
    parallelFib: 122641.1 us
    parallelFib is 284% faster than normalFib


    Data size: 30
    normalFib: 3578.1 us
    parallelFib: 3631.4 us
    normalFib is 1% faster than parallelFib


    Data size: 20
    normalFib: 32.7 us
    parallelFib: 31.9 us
    parallelFib is 2% faster than normalFib


    Data size: 10
    normalFib: 3.5 us
    parallelFib: 3.0 us
    parallelFib is 16% faster than normalFib
    * */

  }
}

