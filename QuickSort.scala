import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object QuickSort {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  def quickSort(list: List[Int]): List[Int] =
    list match
      case Nil => list
      case el :: Nil => list
      case pivot :: _ =>
                val tail = list.tail
                quickSort(tail.filter(i => i < pivot)) ::: list.filter(i => i == pivot) ::: quickSort(quickSort(tail.filter(i => i > pivot)))


  def quickSortParallel(list: List[Int]): List[Int] =
    list match
      case Nil => list
      case el :: Nil => list
      case pivot :: _ =>
        val tail = list.tail
        val smallFuture = Future(quickSort(tail.filter(i => i < pivot)))
        val bigFuture = Future(quickSort(tail.filter(i => i > pivot)))


        Await.result(smallFuture, Duration.Inf) ::: list.filter(i => i == pivot) ::: Await.result(bigFuture, Duration.Inf)

}
