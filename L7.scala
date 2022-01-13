package Test

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object L7 {

  def parallel[A, B](TaskA: => A, TaskB: => B): (A, B) = {
    val f1 = Future {
      TaskA
    }
    val f2 = Future {
      TaskB
    }
    val res1 = Await.result(f1, Duration.Inf)
    val res2 = Await.result(f2, Duration.Inf)
    (res1, res2)
  }

  //Fibonacci number
  def fib(num: Int): Long =
    if (num < 2) num
    else fib(num - 1) + fib(num - 2)

  def parallelFib(num: Int): Long = {
    if (num < 40) {
      if (num < 2) num
      else fib(num - 1) + fib(num - 2)
    }
    else {
      val (fib1, fib2) = parallel(parallelFib(num - 1), parallelFib(num - 2))
      fib1 + fib2
    }
  }

  //Merge sort
  def split(list: List[Int]): (List[Int], List[Int]) = {
    val partition = list.length / 2

    @tailrec
    def splitHelper(leftList: List[Int], partition: Int, list: List[Int]): (List[Int], List[Int]) = {
      partition match {
        case 0 => (list.reverse, leftList)
        case n => splitHelper(leftList.tail, n - 1, leftList.head :: list)
      }
    }

    splitHelper(list, partition, Nil)
  }

  def merge(list1: List[Int], list2: List[Int]): List[Int] = {

    def mergeHelper(list1: List[Int], list2: List[Int], result: List[Int]): List[Int] = {
      (list1, list2) match {
        case (Nil, _) => result.reverse ::: list2
        case (_, Nil) => result.reverse ::: list1
        case (h1 :: t1, h2 :: t2) => {
          if h1 > h2 then mergeHelper(list1, t2, h2 :: result)
          else mergeHelper(t1, list2, h1 :: result)
        }
      }
    }

    mergeHelper(list1, list2, Nil)
  }

  def normalMergeSort(listToSort: List[Int]): List[Int] =
    listToSort match
      case Nil => Nil
      case h :: Nil => listToSort
      case list =>
        val (firstHalf, secondHalf) = split(list)
        merge(normalMergeSort(firstHalf), normalMergeSort(secondHalf))

  def parallelMergeSort(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case h :: Nil => list
      case list => {
        val (f, s) = split(list)
        if(list.length< 100) then normalMergeSort(list)
        else{
          val (rf, rs) = parallel(parallelMergeSort(s), parallelMergeSort(f))
          merge(rs, rf)
        }
      }
    }
  }

  def time[A](block: => A) = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    println("Time: " + (t1 - t0) + " ns")
  }


  def FibTest() = {
    println("Fibonacci number")
    time(parallelFib(50))
    time(fib(50))

    //    Dla 10 elementow:
    //    Paralel: Time: 745900 ns
    //    Normal:  Time: 14300 ns
    //    Możemy zobaczyć ze zrównoleglenie nam się nie opłaca dla małych liczb

    //    Dla 50 elementow:
    //    Paralel: Time: 23486202200 ns
    //    Normal:  Time: 51560687900 ns
    //    A dla liczb większych od 40 już możemy otrzymać minimum w 2 razy lepszy wynik dzięki zrównolegleniu

  }

  def mergSortTest() = {

    def generateList(length: Int, minValue: Int, maxValue: Int): List[Int] = {
      def generateList(length: Int, result: List[Int]): List[Int] = {
        length match
          case 0 => result
          case _ => generateList(length - 1, Random.between(minValue, maxValue) :: result)
      }

      generateList(length, Nil)

    }

    val list = generateList(1000, 1,1000)
    time(normalMergeSort(list))
    time(parallelMergeSort(list))




    //    Dla 100 elementow:
    //    Paralel: Time: 3153900 ns
    //    Normal:  Time: 65448400 ns
    //    Dla 300 elementow:
    //    Paralel: Time: 4427900 ns
    //    Normal:  Time: 68652500 ns
    //    Dla 1000 elementow:
    //    Paralel: Time: 6992700 ns
    //    Normal:  Time: 77399800 ns
    //    Dla sortowania małych list zrównoleglenie sortowania totalnie się nie przydaje,
    //    dla dużych list to może być opłacalne zwłaszcza na to z jaka szybkością rośnie czas zrównoleglenia i w zwykłym przypadku.
  }

  def main(args: Array[String]): Unit = {

    mergSortTest()
   // FibTest()
  }

}

