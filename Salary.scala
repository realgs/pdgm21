import scala.annotation.tailrec
import scala.util.Random
import Functions.*
import scala.concurrent._
import ExecutionContext.Implicits.global
import duration._

object Salary {

  class Worker(idX: Int, workHoursX: List[Int], hourlyRateX: Double) {
    var id: Int = idX
    var hourlyRate: Double = hourlyRateX
    var workHours: List[Int] = workHoursX

    override def toString : String =
      "%d %s".format(id, workHours)
  }

  private def generateHours(maxDaysMonthly: Int, maxTimeDaily: Int): List[Int] = {
    val r = new Random()
    @tailrec
    def randomHours(listOfDays: List[Int], days: Int): List[Int] =
      days match
        case 0 => listOfDays
        case _ =>
          val x = (r.nextFloat() * maxTimeDaily + 1).toInt
          randomHours(x :: listOfDays, days - 1)
    randomHours(List(), (r.nextFloat() * maxDaysMonthly + 1).toInt)
  }

  def generateWorkers(amount: Int, maxDaysMonthly: Int, maxTimeDaily: Int): List[Worker] = {
    val r = new Random()
    @tailrec
    def inner(list: List[Worker], amount: Int): List[Worker] =
      amount match
        case 0 => list
        case _ =>
          inner(Worker(amount, generateHours(maxDaysMonthly, maxTimeDaily), r.nextInt(100) + 20)::list, amount - 1)
    inner(List(), amount)
  }

  private def countSalary(listOfHours: List[Int], hourlyRate: Double): Double =
    @tailrec
    def inner(result: Int, list: List[Int]): Int =
      list match
        case h::t => inner(result + h, t)
        case _ => result
    hourlyRate * inner(0, listOfHours)

  def countSalariesSeq(workers: List[Worker]): Double =
    @tailrec
    def inner(result: Double, list: List[Worker]): Double =
      list match
        case h::t => inner(result + countSalary(h.workHours, h.hourlyRate), t)
        case _ => result
    inner(0, workers).ceil
  
  def countSalariesSeqNoTail(workers: List[Worker]): Double =
      workers match
        case h::t => countSalary(h.workHours, h.hourlyRate) + countSalariesSeqNoTail(t)
        case _ => 0

  def countSalariesFuture(workers: List[Worker]): Double =
    val (x, y) = split(workers)
    val a = Future(countSalariesSeq(x))
    val b = Future(countSalariesSeq(y))
    Await.result(a, Duration.Inf) + Await.result(b, Duration.Inf)

  def countSalariesFuture4(workers: List[Worker]): Double =
    val (x, y) = split(workers)
    val(a, b) = split(x)
    val (c, d) = split(y)
    val w = Future(countSalariesSeq(a))
    val e = Future(countSalariesSeq(b))
    val r = Future(countSalariesSeq(c))
    val t = Future(countSalariesSeq(d))
    Await.result(w, Duration.Inf) + Await.result(e, Duration.Inf) + Await.result(r, Duration.Inf) + Await.result(t, Duration.Inf)
}
