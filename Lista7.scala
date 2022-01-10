import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.Random

object Lista7 {
  // function to measure time
  def time[A](func: => A): Long =
    val t1 = System.nanoTime()
    func
    val t2 = System.nanoTime()
    t2 - t1

  val random = Random

  def generateArray(size: Int): Array[Int] =
    if size > 0 then Array.fill(size)(random.between(-1000, 1000))
    else Array[Int]()

  // quicksort
  def swap(tab: Array[Int], i: Int, j: Int): Unit =
    val temp = tab(i)
    tab(i) = tab(j)
    tab(j) = temp

  def choosePivot(tab: Array[Int], m: Int, n: Int): Int =
    tab((m + n) / 2)

  def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) =
    var i = l
    var j = r
    val pivot = choosePivot(tab, l, r)
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
      if j - l < r - i then
        quick(tab, l, j)
        quick(tab, i, r)
      else
        quick(tab, i, r)
        quick(tab, l, j)
    else ()

  def quickWithFutures(tab: Array[Int], l: Int, r: Int): Unit =
    if l < r then
      val (i, j) = partition(tab, l, r)
      if j - l < r - i then
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

  def quicksort(tab: Array[Int]): Unit =
    quick(tab, 0, tab.length - 1)

  def quicksortWithFutures(tab: Array[Int]): Unit =
    quickWithFutures(tab, 0, tab.length - 1)

  def testQuicksort(list: List[Int]): Unit =
    println("Quicksort")
    for (n <- list)
      val array1 = generateArray(n)
      val array2 = array1.clone()
      println("n = " + n)
      printf("normal,   time taken: %10d ns\n", time(quicksort(array1)))
      printf("parallel, time taken: %10d ns\n", time(quicksortWithFutures(array2)))
      println()

  // mergesort
  def merge(list1: List[Int], list2: List[Int], result: List[Int]): List[Int] =
    (list1, list2) match
      case (_, Nil) => result ::: list1
      case (Nil, _) => result ::: list2
      case (h1 :: t1, h2 :: t2) =>  if (h1 < h2) then merge(t1, list2, h1 :: result)
                                    else merge(list1, t2, h2 :: result)
  def mergesort(list: List[Int]): List[Int] =
    list match
      case Nil => Nil
      case head :: Nil => list
      case _ =>
        val (left, right) = list splitAt (list.length / 2)
        merge(mergesort(left), mergesort(right), Nil)

  def mergesortWithFutures(list: List[Int]): List[Int] =
    list match
      case Nil => Nil
      case head :: Nil => list
      case _ =>
        val (left, right) = list splitAt (list.length / 2)
        val f1 = Future(mergesort(left))
        val f2 = Future(mergesort(right))
        val res = (Await.result(f1, Duration.Inf), Await.result(f2, Duration.Inf))
        merge(res._1, res._2, Nil)

  def testMergesort(list: List[Int]): Unit =
    println("Mergesort")
    for (n <- list)
      val array = generateArray(n)
      val list1 = array.toList
      val list2 = array.clone().toList
      println("n = " + n)
      printf("normal,   time taken: %12d ns\n", time(mergesort(list1)))
      printf("parallel, time taken: %12d ns\n", time(mergesortWithFutures(list2)))
      println()

  // binary tree multiplication
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateTree(depth: Int): BT[Double] =
    depth match
      case 0 => Empty
      case _ => Node(random.nextDouble(), generateTree(depth-1), generateTree(depth-1))

  def calculateProductOfNodes(node: BT[Double]): Double =
    node match
      case Empty => 1
      case Node(value, left, right) => value * calculateProductOfNodes(left) * calculateProductOfNodes(right)

  def calculateProductOfNodesWithFutures(node: BT[Double], depth: Int): Double =
    if depth >= 5 then calculateProductOfNodes(node)
    else
      node match
        case Empty => 1
        case Node(value, left, right) =>
          val f1 = Future(calculateProductOfNodesWithFutures(left, depth + 1))
          val f2 = Future(calculateProductOfNodesWithFutures(right, depth + 1))
          val res = (Await.result(f1, Duration.Inf), Await.result(f2, Duration.Inf))
          value * res._1 * res._2

  def testBTMultiplier(depths: List[Int]): Unit =
    println("BT Multiplication")
    for (n <- depths)
      val tree = generateTree(n)
      println("depth = " + n)
      printf("normal,   time taken: %12d ns\n", time(calculateProductOfNodes(tree)))
      printf("parallel, time taken: %12d ns\n", time(calculateProductOfNodesWithFutures(tree, 0)))
      println()

  def main(args: Array[String]): Unit = {
    testQuicksort(List(100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000))
    println()
    testMergesort(List(100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000))
    println()
    testBTMultiplier(List(5, 10, 15, 20, 25))
  }
}
