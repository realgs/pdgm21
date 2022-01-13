import MergeSort.*
import BinTreeOperations.*
import BestSolution.*
import QuickSort.*

import ParallelResearchUtils.*
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object Main {
  def main(args: Array[String]):  Unit = {
//    val toSort = generateList(50000, Nil)
//
//    println(timer(mergesort(isLess, toSort)))
//
//    println(timer(mergesortParallel(isLess, toSort)))


//    val intTree = generateTreeOfDepth(15)
//
//    println(timer(sumOfTree(intTree)))
//    println(timer(sumOfTreeParallel(intTree, 15)))
//
//    println(timer(sumOfTreeFactorial(intTree)))
//    println(timer(sumOfTreeFactorialParallel(intTree, 15)))
//
//
    //val array = Array("0101", "1011", "0111", "1011")
    val arraySeries = generateSeriesArray(10000)
    println("done")

    //println(array.toList)
    println(timer(findBestSolution(arraySeries)))
    println(timer(findBestSolutionParallel(arraySeries)))
//

//    val array = generateArray(800000)
//    val array2 = array.clone()
//    val array1 = array.clone()
//    println(timer(quicksort(array1)))
//    println(timer(quicksortParallel(array2)))
  }
}
