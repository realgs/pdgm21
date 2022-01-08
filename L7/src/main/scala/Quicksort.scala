import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Quicksort {

  def countTime[A](task : => A) : Long = {
    val time = System.nanoTime()
    task
    System.nanoTime() - time
  }

  def quicksortCreateLists(list : List[Int], pivot : Int) : (List[Int], List[Int], List[Int]) = {
    var lower: List[Int] = List()
    var equal: List[Int] = List()
    var higher: List[Int] = List()

    for (number: Int <- list) {
      if number == pivot then equal = number :: equal
      else if number < pivot then lower = number :: lower
      else higher = number :: higher
    }
    (lower, equal, higher)
  }

  def quicksort(list : List[Int]) : List[Int] = {
    if list.isEmpty || list.tail.isEmpty then list
    else {
      val pivot = list(list.length / 2)
      val (lower, equal, higher) = quicksortCreateLists(list, pivot)

      quicksort(lower) ::: equal ::: quicksort(higher)
    }
  }

    def quicksortParallel(list : List[Int], parallelBound : Int) : List[Int] = {
      if  list.isEmpty || list.tail.isEmpty then list
      else if list.length < parallelBound then {
        quicksort(list)
      }
      else {
        val pivot = list(list.length / 2)
        val (lower, equal, higher) = quicksortCreateLists(list, pivot)

        val sortedLower = Future {
          quicksortParallel(lower, parallelBound)
        }
        val sortedHigher = Future {
          quicksortParallel(higher, parallelBound)
        }
        Await.result(sortedLower, Duration.Inf) ::: equal ::: Await.result(sortedHigher, Duration.Inf)
      }
    }

    def randomList(length : Int, bound : Int) : List[Int] = {
      val random = scala.util.Random
      @tailrec
      def randomListHelper(list : List[Int], currentLength : Int) : List[Int] = {
        if currentLength == length then list
        else {
          randomListHelper(random.nextInt(bound) :: list, currentLength + 1)
        }
      }
      randomListHelper(List(), 0)
    }

    def processList(list : List[Int]) : Unit = {
      println("---------------")
      println("Length: " + list.length)
      print("SEQUENTIAL:  ")
      println(countTime(quicksort(list)))
      print("PARALLEL:    ")
      println(countTime(quicksortParallel(list, (list.length * 0.1).toInt)))
      println("")
    }


  def main(args: Array[String]) : Unit = {
    val list0 = randomList(100, 100000)
    val list1 = randomList(1000, 100000)
    val list2 = randomList(10000, 100000)
    val list3 = randomList(100000, 100000)
    val list4 = randomList(1000000, 100000)
    processList(list0)
    processList(list1)
    processList(list2)
    processList(list3)
    processList(list4)
  }
}
