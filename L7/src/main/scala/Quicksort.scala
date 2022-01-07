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


  def main(args: Array[String]) : Unit = {
    val list = randomList(100000, 100000)
    println(countTime({
      var list1 = quicksort(list)
    }))
    println(countTime({
      var list3 = quicksortParallel(list, (list.length * 0.9).toInt)
    }))
  }
}
