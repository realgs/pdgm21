package Lab7

import Lab7.Knapsack.*
import Lab7.LineInText.*
import Lab7.Fibonacci.*


object Test {

  def timeMeasureNanoSeconds[A](block: => A): Unit = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    println("Total time: " + (t1 - t0) + "ns")
  }

  def lineInTextTest(): Unit = {
    println("10 milion passworld")
    print("Normal Text problem: ") //133209200ns
    timeMeasureNanoSeconds(text("10-million-password-list-top-1000000.txt", "10-million-password-list-top-1000000.txt"))
    print("Future Text problem: ") //103535600ns
    timeMeasureNanoSeconds(textFuture("10-million-password-list-top-1000000.txt", "10-million-password-list-top-1000000.txt"))
    println("10 k passworld")
    print("Normal Text problem: ") //19313300ns
    timeMeasureNanoSeconds(text("1-10000.txt", "1-10000.txt"))
    print("Future Text problem: ") //69634800ns
    timeMeasureNanoSeconds(textFuture("1-10000.txt", "1-10000.txt"))


  }

  def knapsackTest(): Unit = {


    val oneThousandElem = 1000
    val firstWeightList = Array.fill(oneThousandElem)(scala.util.Random.nextInt(10000))
    val firstValueList = Array.fill(oneThousandElem)(scala.util.Random.nextInt(10000))

    val fiveHundredElem = 500
    val secondWeightList = Array.fill(fiveHundredElem)(scala.util.Random.nextInt(10000))
    val secondValueList = Array.fill(fiveHundredElem)(scala.util.Random.nextInt(10000))

    val oneHundredElem = 100
    val thirdWeightList = Array.fill(oneHundredElem)(scala.util.Random.nextInt(10000))
    val thirdValueList = Array.fill(oneHundredElem)(scala.util.Random.nextInt(10000))

    val tenElem = 10
    val fourthWeightList = Array.fill(tenElem)(scala.util.Random.nextInt(10000))
    val fourthValueList = Array.fill(tenElem)(scala.util.Random.nextInt(10000))


    println("Knapsack problem for 1000 elements")
    print("Normal Knapsack problem: ") // 432755000ns
    timeMeasureNanoSeconds(knapsack(1000, firstWeightList, firstValueList, oneThousandElem))
    print("Future Knapsack problem: ") // 155620300ns
    timeMeasureNanoSeconds(knapsackFuture(1000, firstWeightList, firstValueList, oneThousandElem))
    println()

    println("Knapsack problem for 500 elements") // 7702100ns
    print("Normal Knapsack problem: ")
    timeMeasureNanoSeconds(knapsack(1000, secondWeightList, secondValueList, fiveHundredElem))
    print("Future Knapsack problem: ") //3426700ns
    timeMeasureNanoSeconds(knapsackFuture(1000, secondWeightList, secondValueList, fiveHundredElem))
    println()

    println("Knapsack problem for 100 elements")
    print("Normal Knapsack problem: ") // 2262700ns
    timeMeasureNanoSeconds(knapsack(1000, thirdWeightList, thirdValueList, oneHundredElem))
    print("Future Knapsack problem: ") //1765400ns
    timeMeasureNanoSeconds(knapsackFuture(1000, thirdWeightList, thirdValueList, oneHundredElem))
    println()

    println("Knapsack problem for 10 elements")
    print("Normal Knapsack problem: ") //1818200ns
    timeMeasureNanoSeconds(knapsack(1000, fourthWeightList, fourthValueList, tenElem))
    print("Future Knapsack problem: ") //1826400ns
    timeMeasureNanoSeconds(knapsackFuture(1000, fourthWeightList, fourthValueList, tenElem))
    println()


  }

  def fibonaccitest(): Unit = {

    println("Fibonacci 10 elem")
    print("Normal problem: ") //2053300ns
    timeMeasureNanoSeconds(fibonacci(10))
    print("Future problem: ") //67897300ns
    timeMeasureNanoSeconds(fibonacciFuture(10))

    println("Fibonacci 30 elem")

    print("Normal problem: ") //5789100ns
    timeMeasureNanoSeconds(fibonacci(30))
    print("Future problem: ") //3011200ns
    timeMeasureNanoSeconds(fibonacciFuture(30))

    println("Fibonacci 50 elem")

    print("Normal problem: ") //64507391700ns
    timeMeasureNanoSeconds(fibonacci(50))
    print("Future problem: ") //39706398200ns
    timeMeasureNanoSeconds(fibonacciFuture(50))


  }

}
