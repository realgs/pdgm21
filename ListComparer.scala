import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ListComparer {

  def createList(maxNumber: Int, amount: Int):List[Int] = {
    val random = scala.util.Random
    val seq = for(i <- 0 to amount) yield random.nextInt(maxNumber)
    seq.toList
  }

  def sum(list: List[Int]):Long =
    def sumHelper(rest: List[Int], sum: Int):Long =
      rest match
        case Nil => sum
        case _ => sumHelper(rest.tail, sum + rest.head)

    sumHelper(list,0)





  def isSumGreater(first: List[Int], second: List[Int]):Boolean = {
    val firstSum = sum(first)
    val secondSum = sum(second)

    //println(s"${firstSum}    ${secondSum}")


    firstSum > secondSum
  }


  def isSumGreaterParallel(first: List[Int], second: List[Int]):Boolean = {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val sumFirstFuture = Future(sum(first))
    val sumSecondFuture = Future(sum(second))

    val resultFirst = Await.result(sumFirstFuture, Duration.Inf)
    val resultSecond = Await.result(sumSecondFuture, Duration.Inf)

    //println(s"${resultFirst}    ${resultSecond}")


    resultFirst > resultSecond

  }
}
