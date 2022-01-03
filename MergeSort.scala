import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object MergeSort {

  def merge[A](predicate:(A, A) => Boolean, leftList: List[A], rightList: List[A]): List[A] =
    (leftList, rightList) match
      case (Nil, rightList) => rightList
      case (leftList, Nil) => leftList
      case (head1 :: tail1, head2 :: tail2) => if predicate(head1, head2) then head1 :: merge(predicate, tail1, rightList) else head2 :: merge(predicate, leftList, tail2)

  def splitList[T](listToDivide: List[T]): (List[T], List[T]) =
    def getFirstList[T](currentListToDivide: List[T], position: Int): List[T] =
      position match
        case 0 => Nil
        case _ => currentListToDivide.head :: getFirstList(currentListToDivide.tail, position - 1)
    def getSecondList[T](currentListToDivide: List[T], position: Int): List[T] =
      position match
        case 0 => currentListToDivide
        case _ => getSecondList(currentListToDivide.tail, position - 1)
    val div = listToDivide.length / 2
    (getFirstList(listToDivide, div), getSecondList(listToDivide, div))

  def parallel[A, B](taskA: => A, taskB: => B): (A,B) =
    val future: Future[B] = Future { taskB }
    val a: A = taskA
    val b: B = Await.result(future, Duration.Inf)
    (a,b)
    
    

  def mergesort[T](predicate: (T, T) => Boolean, listToSort: List[T]): List[T] =
    if listToSort.length == 0 || listToSort.length == 1 then listToSort else {
      val lists = splitList(listToSort)
      merge(predicate, mergesort(predicate, lists._1), mergesort(predicate, lists._2))
    }

  def mergesortParallel[T](predicate: (T, T) => Boolean, listToSort: List[T]): List[T] =
    if listToSort.length == 0 || listToSort.length == 1 then listToSort else {
      val lists = splitList(listToSort)
      val (l1, l2) = parallel(mergesort(predicate, lists._1), mergesort(predicate, lists._2))
      merge(predicate, l1, l2)
    }
}
