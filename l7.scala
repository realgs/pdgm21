import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await

object l7 {

  def swap[A](tab: Array[A], i: Int, j: Int): Unit =
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  def choose_pivot[A](tab: Array[A], m: Int, n: Int): A =
    tab((m+n)/2)

  def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) =
    var i = l
    var j = r
    val pivot = choose_pivot(tab, l, r)
    while i <= j do
      while tab(i) < pivot do i += 1
      while pivot < tab(j) do j -= 1
      if i <= j then
        swap(tab, i, j)
        i += 1
        j -= 1
    (i, j)

  def quick(tab: Array[Int], l: Int, r: Int): Unit =
    if l < r then
      val (i, j) = partition(tab, l, r)

      if j-l < r-i then
        quick(tab, l, j)
        quick(tab, i, r)
      else
        quick(tab, i, r)
        quick(tab, l, j)
    else ()

  /*def parallelQuicksort(tab: Array[Int], l: Int, r: Int): Unit =
    if(tab.length <= 100) then quick(tab, l, r)
    else
    if l < r then
      val (i, j) = partition(tab, l, r)

      if (j - l) < (r - i) then
        val f1 = Future{parallelQuicksort(tab, l, j)}
        val f2 = Future{parallelQuicksort(tab, i, r)}
      else
        val f2 = Future{parallelQuicksort(tab, i, r)}
        val f1 = Future{parallelQuicksort(tab, l, j)}

    else ()


    def quicksortParallel(tab: Array[Int]) =
    parallelQuicksort(tab, 0, tab.length - 1)
    */

  def parallelQuicksort2(tab: Array[Int], l: Int, r: Int): Unit =
    if(tab.length <= 100) then quick(tab, l, r)
    else
    if l < r then
      val (i, j) = partition(tab, l, r)

      if (j - l) < (r - i) then
        val f1 = Future(quick(tab, l, j))
        val f2 = Future(quick(tab, i, r))
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)
      else
        val f1 = Future(quick(tab, i, r))
        val f2 = Future(quick(tab, l, j))
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)

    else ()


  def quicksort(tab: Array[Int]) =
    quick(tab, 0, tab.length - 1)

  def quicksortParallel2(tab: Array[Int]) =
    parallelQuicksort2(tab, 0, tab.length - 1)


// ***************************************************************************************************************************************************
  def mergesort(tab: List[Int]): List[Int] =
    val n = tab.length / 2
    if (n == 0) then tab
    else
      val (left, right) = tab.splitAt(n)
      merge(mergesort(left), mergesort(right))

  def merge(leftList: List[Int], rightList: List[Int]): List[Int] =
    def mergeInner(leftList: List[Int], rightList: List[Int], result: List[Int]): List[Int] =
      (leftList, rightList) match
        case (h1 :: t1, h2 :: t2) => if (h1 < h2) then mergeInner(t1, rightList, h1 :: result) else mergeInner(leftList, t2, h2 :: result)
        case (h1 :: t1, Nil) => mergeInner(t1, rightList, h1 :: result)
        case (Nil, h2 :: t2) => mergeInner(leftList, t2, h2 :: result)
        case (Nil, Nil) => result
    mergeInner(leftList, rightList, Nil).reverse

  /*def parallel[A, B](taskA: Future[A], taskB: Future[B]): (A, B) =
    val result =
      for
        left  <- taskA
        right <- taskB
      yield
        (left, right)
    Await.result(result, Duration.Inf)
  */

  def parallelMergeSort(tab: List[Int]): List[Int] =
    val n = tab.length / 2
    if (n == 0)  then tab
    else
      val (left, right) = tab.splitAt(n)
      val f1 = Future(mergesort(left))
      val f2 = Future(mergesort(right))
      val res = (Await.result(f1, Duration.Inf), Await.result(f2, Duration.Inf))
      merge(res._1, res._2)

//***************************************************************************************************************************************************
  def main(args: Array[String]): Unit = {
    println("quick sort, size 10 000 000")
    var array = Array.fill(10000000)(scala.util.Random.between(-10000, 10000))
    var clonedArray = array.clone()
    var clonedArray2 = array.clone()

    var time = System.nanoTime()
    quicksort(array)
    println("normal:   " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    quicksortParallel2(clonedArray2)
    println("parallel: " + (System.nanoTime() - time).toString)

    println("\nquick sort, size 10 000")
    array = Array.fill(10000)(scala.util.Random.between(-10000, 10000))
    clonedArray = array.clone()
    clonedArray2 = array.clone()

    time = System.nanoTime()
    quicksort(array)
    println("normal:   " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    quicksortParallel2(clonedArray2)
    println("parallel: " + (System.nanoTime() - time).toString)

//  ***************************************************************************************************************************************************
    println("\nmerge sort, size 1 000 000")
    var list1 = List.fill(1000000)(scala.util.Random.between(-10000, 10000))
    var list2 = List(list1:_*)

    time = System.nanoTime()
    list1 = mergesort(list1)
    println("normal:   " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    list2 = parallelMergeSort(list2)
    println("parallel: " + (System.nanoTime() - time).toString)

    //println(list1.sameElements(list2))

    println("\nmerge sort, size 100 000")
    list1 = List.fill(100000)(scala.util.Random.between(-10000, 10000))
    list2 = List(list1:_*)

    time = System.nanoTime()
    list1 = mergesort(list1)
    println("normal:   " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    list2 = parallelMergeSort(list2)
    println("parallel: " + (System.nanoTime() - time).toString)

    println("\nmerge sort, size 100")
    list1 = List.fill(100)(scala.util.Random.between(-10000, 10000))
    list2 = List(list1:_*)

    time = System.nanoTime()
    list1 = mergesort(list1)
    println("normal:   " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    list2 = parallelMergeSort(list2)
    println("parallel: " + (System.nanoTime() - time).toString)

  }
}
