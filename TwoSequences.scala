import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.annotation.tailrec
import ParallelComponents.*

object TwoSequences
{
  def sumTwoSeq(startElem1: BigInt, seq1: BigInt => BigInt, startElem2: BigInt, seq2: BigInt => BigInt, numberOfElements: BigInt): BigInt =
    def sumTwoSeqHelper(elem1: BigInt, elem2: BigInt, index: BigInt, result: BigInt): BigInt =
      index match
        case 0 => result
        case _ => val (res1, res2) = (seq1(elem1), seq2(elem2)); sumTwoSeqHelper(res1, res2, index - 1, result + res1 + res2)
    if numberOfElements >= 0 then sumTwoSeqHelper(startElem1, startElem2, numberOfElements, startElem1 + startElem2) else 0

  def sumSeq(startElem: BigInt, seq: BigInt => BigInt, numberOfElements: BigInt): BigInt =
    def sumSeqHelper(elem: BigInt, index: BigInt, result: BigInt): BigInt =
      index match
        case 0 => result
        case _ => val seqValue = seq(elem); sumSeqHelper(seqValue, index - 1, result + seqValue)
    if numberOfElements >= 0 then sumSeqHelper(startElem, numberOfElements, startElem) else 0

  /*def sumTwoSeqFuture(startElem1: BigInt, seq1: BigInt => BigInt, startElem2: BigInt, seq2: BigInt => BigInt, numberOfElements: BigInt): BigInt =
    val f1 = Future(sumSeq(startElem1, seq1, numberOfElements))
    val f2 = Future(sumSeq(startElem2, seq2, numberOfElements))
    val result = for {
      r1 <- f1
      r2 <- f2
    } yield (r1 + r2)
    Await.result(result, Duration.Inf)*/

  def sumTwoSeqFuture(startElem1: BigInt, seq1: BigInt => BigInt, startElem2: BigInt, seq2: BigInt => BigInt, numberOfElements: BigInt): BigInt =
    val f1 = Future(sumSeq(startElem1, seq1, numberOfElements))
    val f2 = Future(sumSeq(startElem2, seq2, numberOfElements))
    val (sum1, sum2) = ParallelComponents.parallel(f1, f2)
    sum1 + sum2


  def sumTwoSeqParallel(startElem1: BigInt, seq1: BigInt => BigInt, startElem2: BigInt, seq2: BigInt => BigInt, numberOfElements: BigInt): BigInt =
    val (sum1, sum2) = ParallelComponents.parallelName(sumSeq(startElem1, seq1, numberOfElements), sumSeq(startElem2, seq2, numberOfElements))
    sum1 + sum2

  /*def sumTwoSeqParallel(startElem1: BigInt, seq1: BigInt => BigInt, startElem2: BigInt, seq2: BigInt => BigInt, numberOfElements: BigInt): BigInt =
    val threshold1 = Future {
      sumSeq(startElem1, seq1, numberOfElements)
    }
    val threshold2 = Future {
      sumSeq(startElem2, seq2, numberOfElements)
    }
    Await.result(threshold1, Duration.Inf) + Await.result(threshold2, Duration.Inf)*/
}
