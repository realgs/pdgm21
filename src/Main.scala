import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {

	def numberOfPrimesParaller(range: Int): Int=
		if range >= 2 then
			var k = range/4
			val f1 = Future{numberOfPrimesIn(2, k)}
			val f2 = Future{numberOfPrimesIn(k+1, 2*k)}
			val f3 = Future{numberOfPrimesIn(2*k+1, 3*k)}
			val f4 = Future{numberOfPrimesIn(3*k+1, range)}

			val r1= Await.result(f1, Duration.Inf)
			val r2 = Await.result(f2, Duration.Inf)
			val r3 = Await.result(f3, Duration.Inf)
			val r4 = Await.result(f4, Duration.Inf)

			r1+r2+r3+r4
		else 0

	def numberOfPrimesParaller8Threads(range: Int): Int=
		if range >= 2 then
			var k = range/8
			val f1 = Future{numberOfPrimesIn(2, k)}
			val f2 = Future{numberOfPrimesIn(k+1, 2*k)}
			val f3 = Future{numberOfPrimesIn(2*k+1, 3*k)}
			val f4 = Future{numberOfPrimesIn(3*k+1, 4*k)}
			val f5 = Future{numberOfPrimesIn(4*k+1, 5*k)}
			val f6 = Future{numberOfPrimesIn(5*k+1, 6*k)}
			val f7 = Future{numberOfPrimesIn(6*k+1, 7*k)}
			val f8 = Future{numberOfPrimesIn(7*k+1, range)}

			val r1= Await.result(f1, Duration.Inf)
			val r2 = Await.result(f2, Duration.Inf)
			val r3 = Await.result(f3, Duration.Inf)
			val r4 = Await.result(f4, Duration.Inf)
			val r5= Await.result(f5, Duration.Inf)
			val r6 = Await.result(f6, Duration.Inf)
			val r7 = Await.result(f7, Duration.Inf)
			val r8 = Await.result(f8, Duration.Inf)

			r1+r2+r3+r4+r5+r6+r7+r8
		else 0

	def numberOfPrimesSerial(range: Int): Int=
		if range >= 2 then
			numberOfPrimesIn(2, range)
		else 0

	def numberOfPrimesIn(start: Int, end: Int): Int =
		var res = 0
		for(n <- start to end)
			if isPrime(n) then res += 1
		res

	def isPrime(number: Int): Boolean =
		val range = Math.sqrt(number).asInstanceOf[Int]+1
		var result = false
		for (n <- 2 to range)
			result = result || number%n == 0
		!result

	def serialTest(range: Int): Int=
		var  ts = System.nanoTime()
		val s = numberOfPrimesSerial(range)
		ts = System.nanoTime()-ts
		println("Nano time serial: " + ts/1.0e9)
		s

	def parallerTest(range: Int): Int=
		var tp = System.nanoTime()
		val p = numberOfPrimesParaller(range)
		tp = System.nanoTime()-tp
		println("Nano time paraller: " + tp/1.0e9)
		p

	def main(args: Array[String]): Unit =
		val range = 100000000
		parallerTest(range)

}