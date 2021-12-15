package main.paradygmaty
import main.paradygmaty.Utils

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.Random

object Quick {

  private val random = Random

  def generateRandomArray(size: Int, rangeMin: Int = -100, rangeMax: Int = 100): Array[Int] =
    Array.fill(size)(random.between(rangeMin, rangeMax))

  def printArray[A](array: Array[A]): Unit =
    println(array.mkString(", "))

  def swap[A](array: Array[A], i: Int, j: Int): Unit =
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp

  def partition(array: Array[Int], left: Int, right: Int): (Int, Int) =
    var i = left
    var j = right
    val pivot = array( (i+j) / 2 )
    while (i<=j) {
      while (array(i) < pivot) {
        i += 1
      }
      while(array(j) > pivot) {
        j -= 1
      }
      if i <= j then
        swap(array, i, j)
        i += 1
        j -= 1
    }
    (i, j)


  def quick(array: Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val (i, j) = partition(array, left, right)
      if (j-left) < (right-i) then
        quick(array, left, j)
        quick(array, i, right)
      else
        quick(array, i, right)
        quick(array, left, j)

  def quickSort(array: Array[Int]) =
    quick(array, 0, array.length-1)

  //supposed to be "quicker" but is not, just funny name convention
  def quickerQuick(array: Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val (i, j) = partition(array, left, right)
      if (j-left) < (right-i) then
        val fLeft = Future(quickerQuick(array, left, j))
        val fRight = Future(quickerQuick(array, i, right))
        Utils.parallel(fLeft, fRight)
      else
        val fRight = Future(quickerQuick(array, i, right))
        val fLeft = Future(quickerQuick(array, left, j))
        Utils.parallel(fLeft, fRight)


  def parallelQuickort(array: Array[Int]) =
      quickerQuick(array, 0, array.length-1)


  def doubleQuick(array: Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val (i, j) = partition(array, left, right)
      if (j-left) < (right-i) then
        val fLeft = Future(quick(array, left, j))
        val fRight = Future(quick(array, i, right))
        Utils.parallel(fLeft, fRight)
      else
        val fRight = Future(quick(array, i, right))
        val fLeft = Future(quick(array, left, j))
        Utils.parallel(fLeft, fRight)
        
  def doubleQuickSort(array: Array[Int]) =
    doubleQuick(array, 0, array.length-1)
  
}

