import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.annotation.tailrec
import ParallelComponents.*


object MergeSort
{
  private val generateRandom = Random

  def generateList(length: Int, minValue: Int, maxValue: Int): List[Int] =
    def generateList(length: Int, result: List[Int]): List[Int] =
      length match
        case 0 => result
        case _ => generateList(length - 1, generateRandom.between(minValue, maxValue) :: result)
    generateList(length, Nil)

  def splitPosition[A](list: List[A], position: Int): (List[A], List[A]) =
    @tailrec
    def splitPositionHelper(list: List[A], pos: Int, result: (List[A], List[A])): (List[A], List[A]) =
      (pos, list) match
        case (0, list) => (result._1, list)
        case (_, Nil) => (result._1, Nil)
        case (_, head :: tail) => splitPositionHelper(tail, pos - 1, (head :: result._1, Nil))
    splitPositionHelper(list, position, (Nil, Nil))

  /*def merge(list1: List[Int], list2: List[Int]): List[Int] =
    (list1, list2) match
      case (h1 :: t1, h2 :: t2) => if h1 < h2 then h1 :: merge(t1, list2) else h2 :: merge(list1, t2)
      case (Nil, list2) => list2
      case (list1, Nil) => list1*/

  def merge(list1: List[Int], list2: List[Int]): List[Int] =
    def mergeHelper(list1: List[Int], list2: List[Int], result: List[Int]): List[Int] =
      (list1, list2) match
        case (h1 :: t1, h2 :: t2) => if h1 < h2 then mergeHelper(t1, list2, h1 :: result) else mergeHelper(list1, t2, h2 :: result)
        case (h1 :: t1, Nil) => mergeHelper(t1, list2, h1 :: result)
        case (Nil, h2 :: t2) => mergeHelper(list1, t2, h2 :: result)
        case (Nil, Nil) => result
    mergeHelper(list1, list2, Nil).reverse


  def mergesort(listToSort: List[Int]): List[Int] =
    if listToSort.length < 2 then listToSort
    else
      val (list1, list2) = splitPosition(listToSort, listToSort.length / 2)
      merge(mergesort(list1), mergesort(list2))

  def mergesortAlwaysFuture(listToSort: List[Int]): List[Int] =
    if listToSort.length < 2 then listToSort
    else
      val (list1, list2) = splitPosition(listToSort, listToSort.length / 2)
      val f1 = Future(mergesort(list1))
      val f2 = Future(mergesort(list2))
      val (result1, result2) = ParallelComponents.parallel(f1, f2)
      merge(result1, result2)

  def mergesortMixedFuture(listToSort: List[Int], maxDepth: Int): List[Int] =
    if maxDepth <= 0 then mergesort(listToSort)
    else if listToSort.length < 2 then listToSort
    else
      val (list1, list2) = splitPosition(listToSort, listToSort.length / 2)
      val f1 = Future(mergesortMixedFuture(list1, maxDepth - 1))
      val f2 = Future(mergesortMixedFuture(list2, maxDepth - 1))
      val (result1, result2) = ParallelComponents.parallel(f1, f2)
      merge(result1, result2)

  def mergesortMixedParallel(listToSort: List[Int], maxDepth: Int): List[Int] =
    if maxDepth <= 0 then mergesort(listToSort)
    else if listToSort.length < 2 then listToSort
    else
      val (list1, list2) = splitPosition(listToSort, listToSort.length / 2)
      val (result1, result2) = ParallelComponents.parallelName(mergesortMixedParallel(list1, maxDepth - 1), mergesortMixedParallel(list2, maxDepth - 1))
      merge(result1, result2)
}
