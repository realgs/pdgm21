import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

object MergeSort {

  val random = Random
  def generateListToSort(listLength: Int): List[Int] =
    def helper(elementsLeft: Int, result: List[Int]): List[Int] =
      elementsLeft match
        case 0 => result
        case n => helper(n - 1, random.nextInt() :: result)
    helper(listLength, Nil)

  def splitList(listToSplit: List[Int]): (List[Int], List[Int]) =
    val firstHalfLength = listToSplit.length / 2
    def helper(leftToSplit: List[Int], elementsFirstHalfLeft: Int, firstHalf: List[Int]): (List[Int], List[Int]) =
      elementsFirstHalfLeft match
        case 0 => (firstHalf.reverse, leftToSplit)
        case n => helper(leftToSplit.tail, n - 1, leftToSplit.head :: firstHalf)
    helper(listToSplit, firstHalfLength, Nil)

  def merge(firstHalf: List[Int], secondHalf: List[Int]): List[Int] =
    def helper(firstHalf: List[Int], secondHalf: List[Int], result: List[Int]): List[Int] =
      (firstHalf, secondHalf) match
        case (Nil, _) => result.reverse ::: secondHalf
        case (_, Nil) => result.reverse ::: firstHalf
        case (h1 :: t1, h2 :: t2) =>
          if h1 > h2 then helper(firstHalf, t2, h2 :: result)
          else helper(t1, secondHalf, h1 :: result)
    helper(firstHalf, secondHalf, Nil)

  def normalMergeSort(listToSort: List[Int]): List[Int] =
    listToSort match
      case Nil => Nil
      case h :: Nil => listToSort
      case list =>
        val (firstHalf, secondHalf) = splitList(list)
        merge(normalMergeSort(firstHalf), normalMergeSort(secondHalf))

  def parallelMergeSort(listToSort: List[Int], splitNumber: Int): List[Int] =
    if splitNumber <= 0 then normalMergeSort(listToSort)
    else
      listToSort match
        case Nil => Nil
        case h::Nil => listToSort
        case list =>
          val (firstHalf, secondHalf) = splitList(list)          
          val futureSecondHalf = Future{parallelMergeSort(secondHalf, splitNumber - 1)}          
          val resultFirstHalf = parallelMergeSort(firstHalf, splitNumber - 1)
          val resultSecondHlaf = Await.result(futureSecondHalf, Duration.Inf)          
          merge(resultFirstHalf, resultSecondHlaf)          
}
