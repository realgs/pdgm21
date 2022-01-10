import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source
import com.sksamuel.scrimage.*
import com.sksamuel.scrimage.filter.*
import com.sksamuel.scrimage.nio.{ImmutableImageLoader, PngWriter}

import java.awt.Color
import java.io.File

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

	def primesSerialTest(range: Int): Int=
		var  ts = System.nanoTime()
		val s = numberOfPrimesSerial(range)
		ts = System.nanoTime()-ts
		println("Nano time serial: " + ts/1.0e9)
		s

	def primesParallerTest(range: Int): Int=
		var tp = System.nanoTime()
		val p = numberOfPrimesParaller(range)
		tp = System.nanoTime()-tp
		println("Nano time paraller: " + tp/1.0e9)
		p

	def piIndexOfSerial(substring: String, file: String): Int =
		var occurences = 0
		val digits = Source.fromFile(file).getLines().foldLeft("")((digits, line)=> digits.concat(line))

		for(n <- 0 to digits.length-substring.length)
			if digits.charAt(n) == substring.charAt(0) then
				var bool = true
				for(k <- 1 until substring.length)
					bool &= digits.charAt(n+k) == substring.charAt(k)
				if bool then
					occurences += 1
		occurences

	def piIndexOfParaller(substring: String, file: String): Int =
		val digits = Source.fromFile(file).getLines().foldLeft("")((digits, line)=> digits.concat(line))

		def occurs(start: Int, end: Int): Int=
			var occurences = 0
			for(n <- start to end)
				if digits.charAt(n) == substring.charAt(0) then
					var bool = true
					for(k <- 1 until substring.length)
						bool &= (digits.charAt(n+k) == substring.charAt(k))
					if bool then
						occurences += 1
			occurences

		val k = digits.length/4
		val f1 = Future{occurs(0, k)}
		val f2 = Future{occurs(k, 2*k)}
		val f3 = Future{occurs(2*k, 3*k)}
		val f4 = Future{occurs(3*k, digits.length-substring.length)}
		Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf)+ Await.result(f3, Duration.Inf)+Await.result(f4, Duration.Inf)

	def occursSerialTest(substing: String, file: String): Int=
		var  ts = System.nanoTime()
		val s = piIndexOfSerial(substing, file)
		ts = System.nanoTime()-ts
		println("Occurs time serial: " + ts/1.0e9)
		s

	def occursParallerTest(substring: String, file: String): Int=
		var tp = System.nanoTime()
		val p = piIndexOfParaller(substring, file)
		tp = System.nanoTime()-tp
		println("Occurs time paraller: " + tp/1.0e9)
		p

	def imageProcessing(): Unit=
		val image = ImmutableImage.loader().fromFile("test.jpg")
		val brighted = image.brightness(0.9)
		val resized = brighted.resizeTo(1920,1920, Position.Center)
		val fitted = resized.fit(2560,1920, Color.GREEN)
		val filtered = fitted.filter(new SepiaFilter)
		filtered.output(PngWriter.NoCompression, new File("images/output/modified.png"))


	def main(args: Array[String]): Unit =
//		val range = 100000000
//		parallerTest(range)
//	occursSerialTest("1415", "pi-10million.txt")
//	occursParallerTest("1415", "pi-10million.txt")
	imageProcessing()
}