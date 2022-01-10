import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import Utils.{time, parallel}
import scala.annotation.tailrec

object Mergesort {

  def generateNLongList(N: Int): List[Int] =
    @tailrec
    def generateNLongListRec(N: Int, resultList: List[Int]): List[Int] =
      if N > 0 then generateNLongListRec(N - 1, scala.util.Random.nextInt() :: resultList)
      else resultList
    generateNLongListRec(N, List())


  def split[A](n: Int, list: List[A]): (List[A],List[A]) =
    def splitTail[A](n: Int, list: List[A], leftResultList: List[A], rightResultList: List[A]): (List[A],List[A]) =
      if n > 0 then splitTail (n - 1, list.tail, list.head :: leftResultList, rightResultList)
      else (leftResultList.reverse, list)
    splitTail (n, list, List(), List());;


  def merge[A](order: A => A => Boolean, leftList: List[A], rightList: List[A]): List[A]=
    @tailrec
    def mergeRec[A](order: A => A => Boolean, leftList: List[A], rightList: List[A], resultList: List[A]): List[A]=
      (leftList, rightList) match
        case (h1 :: t1, h2 :: t2) =>
          if(order (h2)(h1)) then mergeRec(order, leftList, t2, h2 :: resultList)
          else mergeRec(order, t1, rightList, h1 :: resultList)
        case (Nil, h2 :: t2) => mergeRec(order, leftList, t2, h2 :: resultList)
        case (h1 :: t1, Nil) => mergeRec(order, t1, rightList, h1 :: resultList)
        case (Nil, Nil) => resultList.reverse
    mergeRec(order, leftList, rightList, List())


  def mergesort[A](order: A => A => Boolean, list: List[A]): List[A]=
    list match
      case Nil => Nil
      case h :: Nil => list
      case _ =>
        val (leftList, rightList) = split (list.length / 2, list)
        merge(order, mergesort(order, leftList), mergesort(order, rightList))


  def mergesortFuture[A](order: A => A => Boolean, list: List[A], threshold: Int): List[A]=
    if threshold <= 0 then mergesort(order, list)
    else
      list match
        case Nil => Nil
        case h :: Nil => list
        case _ =>
          val (leftList, rightList) = split (list.length / 2, list)
          val left = Future {mergesortFuture(order, leftList, threshold - 1)}
          val right = Future {mergesortFuture(order, rightList, threshold - 1)}
          merge(order, Await.result(left, Duration.Inf), Await.result(right, Duration.Inf))


  def mergesortParallel[A](order: A => A => Boolean, list: List[A], threshold: Int): List[A]=
    if threshold <= 0 then mergesort(order, list)
    else
      list match
        case Nil => Nil
        case h :: Nil => list
        case _ =>
          val (leftList, rightList) = split (list.length / 2, list)
          val (left, right) = parallel(mergesortParallel(order, leftList, threshold - 1), mergesortParallel(order, rightList, threshold - 1))
          merge(order, left, right)


  def main(args: Array[String]): Unit = {

    println("Testy sortowania przez scalanie:")
    println()

    val orderInt =  (x: Int) => (y: Int) => x < y

    println("Testy poprawnosci: ")
    println("normalny: ")
    println(mergesort(orderInt, List(1,2,5,9,-3)) == List(-3,1,2,5,9))
    println(mergesort(orderInt, List()) == List())
    println(mergesort(orderInt, List(1,2,5,9,10)) == List(1,2,5,9,10))
    println(mergesort(orderInt, List(10,8,4,2,0)) == List(0,2,4,8,10))
    println()

    println("future: ")
    println(mergesortFuture(orderInt, List(1,2,5,9,-3), 1) == List(-3,1,2,5,9))
    println(mergesortFuture(orderInt, List(), 1) == List())
    println(mergesortFuture(orderInt, List(1,2,5,9,10), 1) == List(1,2,5,9,10))
    println(mergesortFuture(orderInt, List(10,8,4,2,0), 1) == List(0,2,4,8,10))
    println()

    println("parallel: ")
    println(mergesortParallel(orderInt, List(1,2,5,9,-3), 1) == List(-3,1,2,5,9))
    println(mergesortParallel(orderInt, List(), 1) == List())
    println(mergesortParallel(orderInt, List(1,2,5,9,10), 1) == List(1,2,5,9,10))
    println(mergesortParallel(orderInt, List(10,8,4,2,0), 1) == List(0,2,4,8,10))
    println()

    println("Testy przy zmiennym threshold i stalej dlugosci listy:")
    var list =generateNLongList(100000)

    println("length: 100000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    println("length: 100000, threshold: 2")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 2)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 2)))
    println()

    println("length: 100000, threshold: 3")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 3)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 3)))
    println()

    println("length: 100000, threshold: 4")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 4)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 4)))
    println()

    println("length: 100000, threshold: 5")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 5)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 5)))
    println()

    println("length: 100000, threshold: 6")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 6)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 6)))
    println()

    println("length: 100000, threshold: 7")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 7)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 7)))
    println()

    println("length: 100000, threshold: 8")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 8)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 8)))
    println()

    println("length: 100000, threshold: 9")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 9)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 9)))
    println()

    println("Testy przy stalym threshold i zmiennej dlugosci listy:")
    list =generateNLongList(10)
    println("length: 10, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(50)
    println("length: 50, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(100)
    println("length: 100, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(500)
    println("length: 500, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(1000)
    println("length: 1000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(5000)
    println("length: 5000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(10000)
    println("length: 10000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(50000)
    println("length: 50000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(100000)
    println("length: 100000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(500000)
    println("length: 500000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(1000000)
    println("length: 1000000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

    list = generateNLongList(5000000)
    println("length: 5000000, threshold: 1")
    println("normal:   " + time(mergesort(orderInt, list)))
    println("future:   " + time(mergesortFuture(orderInt, list, 1)))
    println("parallel: " + time(mergesortParallel(orderInt, list, 1)))
    println()

  }
}
