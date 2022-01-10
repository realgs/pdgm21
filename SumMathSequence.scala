import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object SumMathSequence {

  def elementOfSequence(n: Int): Double =
    math.pow(-1, n) * (2 - n) / n

  def sumPartMathSequence(firstElement: Int, lastElement: Int): Double =
    def helper(currentElement: Int, result: Double): Double =
      if currentElement > lastElement then result
      else helper(currentElement + 1, result + elementOfSequence(currentElement))
    helper(firstElement, 0)

  def normalSumMathSequence(sequenceLength: Int): Double =
    sumPartMathSequence(1, sequenceLength)

  def parallelSumMathSequence(seqeuenceLength: Int, splitNumber: Int): Double =
    def helper (firstElement: Int, lastElement: Int, splitLeft: Int): Double =
      if splitLeft <= 0 then sumPartMathSequence(firstElement, lastElement)
      else if firstElement == lastElement then elementOfSequence(firstElement)
      else
        val middle = firstElement + (lastElement - firstElement) / 2
        val futureSecondHalf = Future{helper(middle + 1, lastElement, splitLeft - 1)}
        val firstHalfSum = helper(firstElement, middle, splitLeft - 1)
        val secondHalfSum = Await.result(futureSecondHalf, Duration.Inf)
        firstHalfSum + secondHalfSum
    helper(1, seqeuenceLength, splitNumber)
}
