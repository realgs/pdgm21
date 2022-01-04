
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.Random

object Lab7 {

  def swap(tab: Array[Int], i: Int, j: Int) : Unit =
  {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  def choosePivot(table: Array[Int], m: Int, n: Int) =
    table((m+n)/2)

  def partition(tab: Array[Int], l: Int, r: Int) : (Int,Int) =
  {
    var i = l;
    var j = r;
    val pivot = choosePivot(tab, l, r)
    while (i <= j) do
      while (tab(i) < pivot) do i += 1
      while (pivot < tab(j)) do j -= 1
      if (i <= j) then
        swap(tab, i, j)
        i += 1
        j -= 1
    (i, j)
  }


  def quick(tab: Array[Int], l: Int, r: Int): Unit =
    if (l<r) then
      val (i, j) = partition(tab, l, r)
      if (j - l) < (r - i) then
        quick(tab, l, j)
        quick(tab, i, r)
      else
        quick(tab, i, r)
        quick(tab, l, j)
    else ()

  def parallelQuick(tab: Array[Int], l: Int, r: Int): Unit =
    if(tab.length <= 100) then quick(tab, l, r)
    else
    if (l<r) then
      val (i, j) = partition(tab, l, r)
      if (j - l) < (r - i) then

        val left = Future(parallelQuick(tab, l, j))
        val right = Future(parallelQuick(tab, i, r))
        Await.result(left, Duration.Inf)
        Await.result(right, Duration.Inf)

      else
        val right = Future(parallelQuick(tab, i, r))
        val left = Future(parallelQuick(tab, l, j))
        Await.result(right, Duration.Inf)
        Await.result(left, Duration.Inf)

    else ()

  def quicksort(tab: Array[Int]) =
    quick(tab, 0, tab.length - 1)

  def parallelQuicksort(tab: Array[Int]) =
    parallelQuick(tab, 0, tab.length - 1)

  val r = scala.util.Random
  def createArray(size: Int) : Array[Int] =
    if(size > 0) Array.fill(size)(r.between(-10000, 10000))
    else Array()


  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]


  def randomTree (levels : Int) : BinaryTree[Double] =
    if (levels > 0) then Node(Random.nextDouble(), randomTree(levels - 1), randomTree(levels - 1))
    else Empty



  def multiplyTree(tree: BinaryTree[Double]): Double =
      tree match
        case Empty => 1
        case Node(v, l, r) =>
             v * multiplyTree(l) * multiplyTree(r)


  //najlepiej działa od poziomu 15, natomiast od 11 widać różnice, poniżej nie opłaca się używać zrównoleglonej funkcji
  def parallelMultiplyTree(binaryTree: BinaryTree[Double], maxLevel: Int = 15): Double =
    def helper(tree: BinaryTree[Double], currentLevel: Int): Double =
      if currentLevel <= maxLevel then multiplyTree(tree)
      else
        tree match
          case Empty => 1
          case Node(v, l, r) =>
            val left = Future { helper(l, currentLevel-1) }
            val right = Future { helper(r, currentLevel-1) }
            val result =
              for
                leftR <- left
                rightR <- right
              yield
                (leftR * rightR * v)
            Await.result(result, Duration.Inf)
    helper(binaryTree, maxLevel)

   def merge(leftList: List[Int], rightList: List[Int]): List[Int] =
     (leftList, rightList) match {
            case (Nil, Nil) => Nil
            case(leftList, Nil) => leftList
            case(Nil, rightList) => rightList
            case(hl :: tl, hr :: tr) =>  if (hl  < hr) hl :: merge(tl, rightList)
                                         else hr :: merge(leftList, tr)
    }

  def split(list : List[Int], n : Int) : (List[Int], List[Int]) =
    @tailrec
    def helper(currentList : List[Int], leftList : List[Int], rightList : List[Int], counter : Int) : (List[Int], List[Int]) =
      currentList match
        case Nil => (leftList, rightList)
        case(h :: t) => if(counter > 0) then helper(t, h :: leftList, rightList, counter - 1)
                        else (leftList, currentList)
    helper(list, Nil, Nil, n)

  def mergeSort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = split(list, n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def parallel[A, B](taskA: Future[A], taskB: Future[B]): (A, B) =
    val result =
      for
        left <- taskA
        right <- taskB
      yield
        (left, right)

    Await.result(result, Duration.Inf)

  def parallelMergeSort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    //if(list.length <= 10) then mergeSort(list)
    //else
      if (n == 0)  then list
      else {
         val (left, right) = split(list, n)
         val firstMerge = Future(mergeSort(left))
         val secondMerge = Future(mergeSort(right))
         val (firstResult, secondResult) = parallel(firstMerge, secondMerge)
         merge(firstResult, secondResult)
    }
  }

  def createList(size: Int) : List[Int] =
    if(size > 0) then r.between(-10000, 10000) :: createList(size -1)
    else List()

  def main(args: Array[String]): Unit = {
    println("Quick Sort")
    println("Size 10")
    var firstArray = createArray(10)
    var secondArray = firstArray.clone()
    var time = System.currentTimeMillis()
    quicksort(firstArray)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
   // println(firstArray.toList)
    time = System.currentTimeMillis()
    parallelQuicksort(secondArray)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
   // println(secondArray.toList)
    println("Size 100")
    firstArray = createArray(100)
    secondArray = firstArray.clone()
    time = System.currentTimeMillis()
    quicksort(firstArray)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    // println(firstArray.toList)
    time = System.currentTimeMillis()
    parallelQuicksort(secondArray)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    // println(secondArray.toList)
    println("Size 1000")
    firstArray = createArray(1000)
    secondArray = firstArray.clone()
    time = System.currentTimeMillis()
    quicksort(firstArray)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    // println(firstArray.toList)
    time = System.currentTimeMillis()
   // parallelQuicksort(secondArray)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    // println(secondArray.toList)
    println("Size 10000")
    firstArray = createArray(10000)
    secondArray = firstArray.clone()
    time = System.currentTimeMillis()
    quicksort(firstArray)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    // println(firstArray.toList)
    time = System.currentTimeMillis()
   // parallelQuicksort(secondArray)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    // println(secondArray.toList)


    println("Multiply tree")
    println("Levels : 5")
    var tree = randomTree(5)
    time = System.currentTimeMillis()
    multiplyTree(tree)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMultiplyTree(tree)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Levels : 10")
    tree = randomTree(10)
    time = System.currentTimeMillis()
    multiplyTree(tree)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMultiplyTree(tree)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Levels : 15")
    tree = randomTree(15)
    time = System.currentTimeMillis()
    multiplyTree(tree)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMultiplyTree(tree)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Levels : 15")
    tree = randomTree(15)
    time = System.currentTimeMillis()
    multiplyTree(tree)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMultiplyTree(tree)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Levels : 20")
    tree = randomTree(20)
    time = System.currentTimeMillis()
    multiplyTree(tree)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMultiplyTree(tree)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Levels : 25")
    tree = randomTree(25)
    time = System.currentTimeMillis()
    multiplyTree(tree)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMultiplyTree(tree)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)

    println("Merge sort")
    println("Size 10")
    var list  = createList(10)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Size 100")
    list  = createList(100)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Size 300")
    list  = createList(300)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Size 500")
    list  = createList(500)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)

    println("Size 700")
    list  = createList(700)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Size 1000")
    list  = createList(1000)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)
    println("Size 2000")
    list  = createList(2000)
    time = System.currentTimeMillis()
    mergeSort(list)
    println("Time without parallel: " + (System.currentTimeMillis() - time).toString)
    time = System.currentTimeMillis()
    parallelMergeSort(list)
    println("Time with parallel: " + (System.currentTimeMillis() - time).toString)


  }
}
