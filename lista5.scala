import scala.annotation.tailrec
import scala.util.Random

object Main {
  
  // Zadanie 1

  def decimalToHex(numberToConvert: Int): List[Int] =
    @tailrec
    def convert(currentNumber: Int, result: List[Int]): List[Int] =
      if (currentNumber == 0) then 
        result
      else 
        convert(currentNumber / 16, (currentNumber%16) :: result)

    if (numberToConvert == 0) then
      List(0)
    else 
      convert(numberToConvert, List())

  


  def decimalToAny(numberToConvert: Int, system: Int) = 
    @tailrec
    def convert(currentNumber: Int, result: List[Int]): List[Int] =
      if (currentNumber == 0) then 
        result
      else 
        convert(currentNumber / system, (currentNumber%system) :: result)

    if (system < 2) then
      throw new RuntimeException("System can't be lower than 2")
    if (numberToConvert == 0) then
      List(0)
    else 
      convert(numberToConvert, List())

  



  // Zadanie 3

  sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


  def createTree(N: Int): BT[Double] =
    val random = Random()
    if N <= 0 then Empty
    else Node(random.nextDouble(), createTree(N-1), createTree(N-1))

  


  // Zadanie 4
  def multiplyTree(tree: BT[Double]) : Double = 
    tree match
        case Empty => 1
        case Node(element, leftSubTree, rightSubTree) => element * multiplyTree(leftSubTree) * multiplyTree(rightSubTree)


  
  
  def main(args: Array[String]): Unit = {
    println(decimalToHex(31))
    println(decimalToHex(-31))
    println(decimalToHex(0))
    println(decimalToHex(725))
    println(decimalToAny(15, 2))
    //  println(decimalToAny(13, 1))  //throws Exception
    println(decimalToAny(254, 16))
    println(decimalToAny(9, 3))
    println(createTree(3))
    println(multiplyTree(createTree(3)))
  }
  
}
