import scala.concurrent.Future
import scala.util.Random

object List7 {

  def swap(tab:Array[Int], i: Int, j: Int): Unit =
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  def partition(tab:Array[Int], l: Int, r: Int): (Int, Int) =
    var i = l
    var j = r
    val pivot = tab((l + r) / 2)
    while i <= j do
      while tab(i) < pivot do i += 1
      while pivot < tab(j) do j -= 1
      if i <= j then
        swap(tab, i, j)
        i+=1
        j-=1
    (i,j)

  def quick(tab:Array[Int], l: Int, r: Int): Unit =
    if l < r then
      val (i, j) = partition(tab, l, r)
      if j - l < r - i then
        quick (tab, l, j)
        quick (tab, i, r)
      else
        quick (tab, i, r)
        quick (tab, l, j)
    else ()

  def quicksort(tab: Array[Int]): Unit = quick(tab, 0,tab.length - 1)

  def quickParallel(tab: Array[Int], l: Int, r: Int): Unit=
    if l < r then
      val (i, j) = partition(tab, l, r)
      if j - l < r - i then
        val future1 = Future
          quickParallel(tab, l, j)
        val future2 = Future
          quickParallel(tab, i, r)
      else
        val future1 = Future
          quickParallel(tab, i, r)
        val future2 = Future
          quickParallel(tab, l, j)
    else ()

  def quicksortParallel(tab: Array[Int]): Unit = quickParallel(tab, 0,tab.length - 1)

  def quickParallel2(tab: Array[Int], l: Int, r: Int): Unit=
    if l < r then
      val (i, j) = partition(tab, l, r)
      if j - l < r - i then
        val future1 = Future
          quick(tab, l, j)
        val future2 = Future
          quick(tab, i, r)
      else
        val future1 = Future
          quick(tab, i, r)
        val future2 = Future
          quick(tab, l, j)
    else ()

  def quicksortParallel2(tab: Array[Int]): Unit = quickParallel2(tab, 0,tab.length - 1)

  def quickParallel3(tab: Array[Int], l: Int, r: Int): Unit=
    if l < r then
      val (i, j) = partition(tab, l, r)
      if j - l < r - i then
        val future1 = Future
        quick(tab, l, j)
        val future2 = Future
        quickParallel(tab, i, r)
      else
        val future1 = Future
        quick(tab, i, r)
        val future2 = Future
        quickParallel(tab, l, j)
    else ()

  def quicksortParallel3(tab: Array[Int]): Unit = quickParallel3(tab, 0,tab.length - 1)

  def main(args: Array[String]): Unit = {


    var array0 = Array.fill(10)(Random.nextInt(10))
    var array1 = array0.clone()
    var array2 = array0.clone()
    var array3 = array0.clone()

    var start = System.currentTimeMillis()
    var end = System.currentTimeMillis()

    println("10 elems")
    start = System.currentTimeMillis()
    quicksort(array0)
    end = System.currentTimeMillis()
    println("Time1: " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel(array1)
    end = System.currentTimeMillis()
    println("Time2: " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel2(array2)
    end = System.currentTimeMillis()
    println("Time3 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel3(array3)
    end = System.currentTimeMillis()
    println("Time4 " + (end-start))

    array0 = Array.fill(1000)(Random.nextInt(1000))
    array1 = array0.clone()
    array2 = array0.clone()
    array3 = array0.clone()

    println("1000 elems")

    start = System.currentTimeMillis()
    quicksort(array0)
    end = System.currentTimeMillis()
    println("Time1 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel(array1)
    end = System.currentTimeMillis()
    println("Time2 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel2(array2)
    end = System.currentTimeMillis()
    println("Time3 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel3(array3)
    end = System.currentTimeMillis()
    println("Time4 " + (end-start))

    array0 = Array.fill(1000000)(Random.nextInt(1000000))
    array1 = array0.clone()
    array2 = array0.clone()
    array3 = array0.clone()

    println("1000000 elems")

    start = System.currentTimeMillis()
    quicksort(array0)
    end = System.currentTimeMillis()
    println("Time1 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel(array1)
    end = System.currentTimeMillis()
    println("Time2 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel2(array2)
    end = System.currentTimeMillis()
    println("Time3 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel3(array3)
    end = System.currentTimeMillis()
    println("Time4 " + (end-start))

    array0 = Array.fill(10000000)(Random.nextInt(1000000))
    array1 = array0.clone()
    array2 = array0.clone()
    array3 = array0.clone()

    println("10000000 elems")

    start = System.currentTimeMillis()
    quicksort(array0)
    end = System.currentTimeMillis()
    println("Time1 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel(array1)
    end = System.currentTimeMillis()
    println("Time2 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel2(array2)
    end = System.currentTimeMillis()
    println("Time3 " + (end-start))

    start = System.currentTimeMillis()
    quicksortParallel3(array3)
    end = System.currentTimeMillis()
    println("Time4 " + (end-start))

  }

  }
