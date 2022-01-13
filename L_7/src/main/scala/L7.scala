import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object L7 {

    def arrayGenerator(size: Int): Array[Int] =
      val array = new Array[Int](size)
      for(i <- 0 until size)
        array(i) = Random.nextInt()
      array

    // 1.
    def sumOfArrayElements(array: Array[Int]): Int =
      var sum = 0
      for(i <- 0 until array.length)
        sum += array(i)
      sum

    def sumOfArrayElements_P(array: Array[Int]): Int =

      def helper(firstIndex: Int, lastIndex: Int): Int =
        var sum = 0
        for(i <- firstIndex until lastIndex)
          sum += array(i)
        sum

      val l = array.length

      val f1 = Future{ helper(0, l/8) }
      val f2 = Future{ helper(l/8, l/4) }
      val f3 = Future{ helper(l/4, l*3/8) }
      val f4 = Future{ helper(l*3/8, l/2) }
      val f5 = Future{ helper(l/2, l*5/8) }
      val f6 = Future{ helper(l*5/8, l*3/4) }
      val f7 = Future{ helper(l*3/4, l*7/8) }
      val f8 = Future{ helper(l*7/8, l) }

      Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) + Await.result(f4, Duration.Inf) +Await.result(f5, Duration.Inf) + Await.result(f6, Duration.Inf) + Await.result(f7, Duration.Inf) + Await.result(f8, Duration.Inf)

    // 2.
    def someOperations(elem: Int): Int =
      var e = elem
      e %= 17
      e /= 2
      e += 1
      e *= 15
      e -=7
      e *= 3
      e /= 2
      e %= 2
      e += 9
      e *= 7
      e

    def doSomeOperationsOnEachElement(array: Array[Int]): Array[Int] =
      for(i <- 0 until array.length)
        array(i) = someOperations(array(i))
      array

    def doSomeOperationsOnEachElement_P(array: Array[Int]): Array[Int] =
      def helper(firstElem: Int, lastElem: Int): Array[Int] =
        for(i <- firstElem until lastElem)
          array(i) = someOperations(array(i))
        array

      val l = array.length

      val f1 = Future{ helper(0, l/4) }
      val f2 = Future{ helper(l/4, l/2) }
      val f3 = Future{ helper(l/2, l*3/4) }
      val f4 = Future{ helper(l*3/4, l) }

      Await.result(f1, Duration.Inf)
      Await.result(f2, Duration.Inf)
      Await.result(f3, Duration.Inf)
      Await.result(f4, Duration.Inf)

    // 3.
    def swap[A](tab: Array[A])(i: Int)(j: Int) =
      val aux = tab(i)
      tab(i) = tab(j)
      tab(j) = aux

    def choose_pivot[A](tab: Array[A])(m: Int)(n: Int) =
      tab((m+n)/2)

    def partition(tab: Array[Int])(l: Int)(r: Int) =
      var i = l
      var j = r
      val pivot = choose_pivot(tab)(l)(r)
      while (i <= j)
        while (tab(i) < pivot) i += 1
        while (pivot < tab(j)) j -= 1
        if (i <= j)
          swap(tab)(i)(j)
          i += 1
          j -= 1
      (i, j)

    def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
      if (l < r)
        val (i, j) = partition(tab)(l)(r)
        if (j - l < r - i)
          val _ = quick(tab)(l)(j)
          quick(tab)(i)(r)
        else
          val _ = quick(tab)(i)(r)
          quick(tab)(l)(j)

    def quick_P(tab: Array[Int])(l: Int)(r: Int): Unit =
      if l < r then
        val (i,j) = partition(tab)(l)(r)
        if (j - l < r - i)
          val f1 = Future(quick(tab)(l)(j))
          val f2 = Future(quick(tab)(i)(r))
          Await.result(f1, Duration.Inf)
          Await.result(f2, Duration.Inf)
        else
          val f1 = Future(quick(tab)(i)(r))
          val f2 = Future(quick(tab)(l)(j))
          Await.result(f1, Duration.Inf)
          Await.result(f2, Duration.Inf)

    def quicksort(tab: Array[Int]) = quick(tab)(0)(tab.length - 1)

    def quicksort_P(tab: Array[Int]): Unit = quick_P(tab)(0)(tab.length - 1)

  def main(args: Array[String]): Unit ={

    val array = arrayGenerator(100000000)

    val t1 = System.currentTimeMillis()
    sumOfArrayElements(array)
    val t2 = System.currentTimeMillis()
    println(t2-t1)
    val t3 = System.currentTimeMillis()
    sumOfArrayElements_P(array)
    val t4 = System.currentTimeMillis()
    println(t4-t3)

    //Sequential vs Parallel
    //(100.000.000 elems each case)
    //f 1-4: 462 - 512
    //f 1-8: 468 - 415

    val t5 = System.currentTimeMillis()
    doSomeOperationsOnEachElement(array)
    val t6 = System.currentTimeMillis()
    println(t6-t5)
    val t7 = System.currentTimeMillis()
    doSomeOperationsOnEachElement_P(array)
    val t8 = System.currentTimeMillis()
    println(t8 - t7)

    //Sequential vs Parallel
    //(100.000.000 elems each case)
    //3 ops: 525 - 531
    //10 ops: 871 - 680

    val t9 = System.currentTimeMillis()
    quicksort(array)
    val t10 = System.currentTimeMillis()
    println(t10-t9)
    val t11 = System.currentTimeMillis()
    quicksort_P(array)
    val t12 = System.currentTimeMillis()
    println(t12 - t11)

    //Sequential vs Parallel
    //100.000 elems: 38 - 57
    //1.000.000 elems: 142 - 61
    //10.000.000 elems: 1356 - 194
    //100.000.000 elems: 16245 - 1493

  }
}
