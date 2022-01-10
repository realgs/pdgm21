import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.Random
import Functions.*
import ExecutionContext.Implicits.global
import scala.annotation.tailrec

object MonteCarloPi {

def count(points: Double): Double = {
  val randomX = new Random()
  val randomY = new Random()
  var hits = 0
  var i = 0
  while(i < points) {
    val x = randomX.nextDouble()
    val y = randomY.nextDouble()
    if (x * x + y * y < 1) hits = hits + 1
    i+=1
  }
  hits
}

//  private def count(points: Double): Double = {
//    var hits = 0
//    var i = 0
//    while (i < points) {
//      val x = Math.random()
//      val y = Math.random()
//      if (x * x + y * y < 1) hits = hits + 1
//      i += 1
//    }
//    hits
//  }

  def findPiSeq(points: Double): Double =
    4.0 * count(points) / points

  def monteCarloPiParallel(points: Double): Double = {
    val x = parallel(parallel(count(points / 4), count(points / 4)), parallel (count(points / 4), count(points / 4)))
    val result = 4.0*(x._1._1+x._1._2+x._2._1+x._2._2)/points
    result
  }

  def findPiFutures(points: Integer, futures: Integer): Double = {
    val f = for (p <- 1 to futures) yield Future {
      count(points/futures)
    }
    val results = f.map(Await.result(_, Duration.Inf))
    4.0 * results.sum / points
  }

  def segmentRec(points: Double, threshold: Int): Double = {
    def inner(points: Double): Double =
      if points < threshold then count(points)
      else
        val (x, y) = parallel(inner(points / 2), inner(points / 2))
        x + y
    4.0 * inner(points) / points
  }
}
