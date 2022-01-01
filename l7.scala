import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Random

object Main {

    val check_circle = 
        (x: Float, y: Float) => { x*x + y*y < 1 }
    
    def calcPi_parallel(n_points: Integer, n_futures: Integer): Double = {
        val futures = for (p <- 1 to n_futures) yield Future {
            val r = new Random
            var count = 0
            for (i <- 1 to (n_points / n_futures))
                if check_circle(r.nextFloat(), r.nextFloat()) then count = count + 1
            count
        }
        val results = futures.map(Await.result(_, duration.Duration.Inf))
        4.0 * (results.sum) / n_points
    }

    def calcPi(n_points: Integer): Double = {
        val r = new Random
        var count = 0
        for (i <- 1 to n_points)
            if check_circle(r.nextFloat(), r.nextFloat()) then count = count + 1
        4.0 * count / n_points
    }

    def main(args: Array[String]): Unit = {
        val t0 = System.nanoTime()
        calcPi_parallel(1000000, 32)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        val t2 = System.nanoTime()
        calcPi(1000000)
        val t3 = System.nanoTime()
        println("Elapsed time: " + (t3 - t2) + "ns")
    }
}
