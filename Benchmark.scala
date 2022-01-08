import scala.annotation.tailrec
import scala.util.Random

object Benchmark {

  def measureTime[T](task: => T): Long =
    val start = System.nanoTime()
    task
    System.nanoTime() - start

  def generateListInt(length: Int, min: Int, max: Int): List[Int] =

    @tailrec
    def generateList(elemLeft: Int, generatedList: List[Int]): List[Int] =
      elemLeft match
        case 0 => generatedList
        case _ => generateList(elemLeft - 1, Random.between(min, max) :: generatedList)
    generateList(length, Nil)
}
