import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.Duration
import scala.util.Random

object l7 {

  def measureTime[A](instruction: => A): Unit =
    val t1 = System.nanoTime()
    instruction
    val t2 = System.nanoTime()
    print("Time: " + (t2 - t1) + " ns" )

  // quicksort
  def swap(tab: Array[Int])(i: Int)(j: Int) = { var aux = tab(i); tab(i) = tab(j); tab(j) = aux }

  def choose_pivot(tab: Array[Int])(m: Int)(n: Int) = tab((m + n) / 2)

  def partition(tab: Array[Int])(l: Int)(r: Int) =
    var i = l; var j = r; val pivot = choose_pivot(tab)(l)(r)
    while i <= j do
      while tab(i) < pivot do i += 1;
      while pivot < tab(j) do j -= 1;
      if i <= j then { swap(tab)(i)(j); i += 1; j -= 1 }
    (i, j)

  def quick (tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j - l < r - i then { quick(tab)(l)(j); quick(tab)(i)(r) }
      else { quick(tab)(i)(r); quick(tab)(l)(j) }
    else ()

  def quickParallel(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j - l < r - i then {
        val result1 = Future(quick(tab)(l)(j))
        val result2 = Future(quick(tab)(i)(r))

        Await.result(result1, Duration.Inf)
        Await.result(result2, Duration.Inf)
      }
      else {
        val result1 = Future(quick(tab)(i)(r))
        val result2 = Future(quick(tab)(l)(j))

        Await.result(result1, Duration.Inf)
        Await.result(result2, Duration.Inf)
      }
    else ()

  def quicksort (tab: Array[Int]) = quick(tab)(0)(tab.length - 1);;

  def quicksortParallel (tab: Array[Int]) = quickParallel(tab)(0)(tab.length - 1);;

  def quicksortCompare =
    val tab1 = Array.fill(1000)(Random.nextInt(1000))
    val tab2 = Array.fill(10000)(Random.nextInt(1000))
    val tab3 = Array.fill(100000)(Random.nextInt(1000))
    val tab4 = Array.fill(1000000)(Random.nextInt(1000))
    val tab5 = Array.fill(10000000)(Random.nextInt(1000))

    println("Quicksorts:")
    println("1000 elements:")
    print("Sequentialy: ")
    measureTime(quicksort(tab1.clone()))
    print("Parallelly: ")
    measureTime(quicksortParallel(tab1.clone()))
    println()

    println("10000 elements:")
    print("Sequentialy: ")
    measureTime(quicksort(tab2.clone()))
    print("Parallelly: ")
    measureTime(quicksortParallel(tab2.clone()))
    println()

    println("100000 elements:")
    print("Sequentialy: ")
    measureTime(quicksort(tab3.clone()))
    print("Parallelly: ")
    measureTime(quicksortParallel(tab3.clone()))
    println()

    println("1000000 elements:")
    print("Sequentialy: ")
    measureTime(quicksort(tab4.clone()))
    print("Parallelly: ")
    measureTime(quicksortParallel(tab4.clone()))
    println()

    println("10000000 elements:")
    print("Sequentialy: ")
    measureTime(quicksort(tab5.clone()))
    print("Parallelly: ")
    measureTime(quicksortParallel(tab5.clone()))
    println()

  

}
