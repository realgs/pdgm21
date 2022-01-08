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

  def generate2DArray(rows: Int, columns: Int, minValue: Int, maxValue: Int): Array[Array[Int]] =
    val array = Array.ofDim[Int](rows, columns)

    for(i <- 0 until rows)
      for(j <- 0 until columns)
        array(i)(j) = Random.between(minValue, maxValue)
    array

  def generateSquareArray(size: Int, minValue: Int, maxValue: Int): Array[Array[Int]] =
    generate2DArray(size, size, minValue, maxValue)
}
