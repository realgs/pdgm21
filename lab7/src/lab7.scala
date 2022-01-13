import jdk.jfr.Threshold

import scala.annotation.tailrec

import scala.concurrent._, ExecutionContext.Implicits.global, duration._

object lab7 {


  def time[T](f: => T): Unit={
    val start = System.nanoTime()
    f
    val end = System.nanoTime()

    println("Time = " + (end - start))
  }


  def normalAddTwoLists(listA: LazyList[Int], listB: LazyList[Int]): LazyList[Int] = {
    (listA, listB) match
      case (LazyList(), _) => LazyList()
      case (_, LazyList()) => LazyList()
      case (hA #:: tA, hB #:: tB) => (hA + hB) #:: normalAddTwoLists(tA, tB)
  }

  def parallelAddTwoLists(listA: LazyList[Int], listB: LazyList[Int]): LazyList[Int] = {

    (listA, listB) match
      case (LazyList(), _) => LazyList()
      case (_, LazyList()) => LazyList()
      case (hA #:: tA, hB #:: tB) => {
        val f2 = Future {
          parallelAddTwoLists(listA.tail, listB.tail)
        }
        val f3 = Future{
          (listA.head + listB.head)
        }
        Await.result(f3, Duration.Inf) #:: Await.result(f2, Duration.Inf)
      }

  }


  //nie skraca czasu?
  def sumList(list: List[Int]): Int = {
    @tailrec
    def sumListHelp(list: List[Int], counter: Int): Int = {
      list match {
        case Nil => counter
        case h :: t => sumListHelp(t, counter+h)
      }
    }

    sumListHelp(list, 0)
  }

  def sumListParallel(list: List[Int]): Int ={
    def sumListParallelHelp(list: List[Int], startIndex: Int, endIndex: Int): Int = {
      var sum = 0
      var counter = startIndex;
      while (counter != endIndex + 1) {
        sum = sum+list(counter)
        counter = counter+1
      }
      sum
    }
    val f1 = Future{ sumListParallelHelp(list, 0, (list.size/4)-1) }
    val f2 = Future{ sumListParallelHelp(list, list.size/4, (list.size * 2/4) - 1) }
    val f3 = Future{ sumListParallelHelp(list, (list.size * 2/4), (list.size * 3/4) -1) }
    val f4 = Future { sumListParallelHelp(list, (list.size * 3/4), list.size-1) }

    Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) + Await.result(f4, Duration.Inf)
  }
  def main(args: Array[String]): Unit = {


    time {
      normalAddTwoLists(LazyList.from(1).take(1000000), LazyList.from(10000).take(1000000))
    }


    time {
      parallelAddTwoLists(LazyList.from(1).take(1000000), LazyList.from(10000).take(1000000))
    }


    time {
      sumList(List.fill(100000)(10))
    }


    time {
      sumListParallel(List.fill(100000)(10))
    }

  }
}
