import scala.annotation.tailrec
import scala.util.Random

object lab5 {

  //Zadanie 1
  //Rekursja ogonowa
  //Dodawanie do listy tak, że nie trzeba pozniej tej listy odwracac
  //Jesli decimalNumber = 0 to nie wykonujemy zadnych zbednych operacji
  def decimalToHex(decimalNumber: Int): List[Int] = {
    @tailrec
    def go(decimalNumber: Int, hexNumber: List[Int]): List[Int] = {
      if decimalNumber == 0 then hexNumber
      else go(decimalNumber / 16, (decimalNumber % 16) :: hexNumber)
    }
    if decimalNumber == 0 then List(0)
    else go(decimalNumber, Nil)
  }

  def decimalToOtherSystem(decimalNumber: Int, system: Int): List[Int] = {
    @tailrec
    def go(decimalNumber: Int, system: Int, resultNumber: List[Int]): List[Int] = {
      if decimalNumber == 0 then resultNumber
      else go(decimalNumber / system, system, (decimalNumber % system) :: resultNumber)
    }
    if decimalNumber == 0 then List(0)
    else go(decimalNumber, system, Nil)
  }

  //Zadanie 3
  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  def generateBinaryTree(N: Int): BinaryTree[Float] = {
    def go(currentN: Int, N: Int): BinaryTree[Float] = {
      val rand = Random()
      if currentN == N then Empty
      else Node(rand.nextFloat(), go(currentN + 1, N), go(currentN + 1, N))
    }
    go(0, N)
  }

  //Zadanie 4
  def valuesMultiplicationInBinaryTree(root: BinaryTree[Float]): Float = {
    def go(node: BinaryTree[Float]): Float =
      node match
        case Empty => 1
        case Node(v, l, r) => v * go(l) * go(r)
    if root == Empty then 0
    else go(root)
  }

  def main(args: Array[String]): Unit = {

    //Zadanie 1
    println(decimalToHex(0) == List(0))
    println(decimalToHex(16) == List(1, 0))
    println(decimalToHex(58) == List(3, 10))

    println(decimalToOtherSystem(0, 2) == List(0))
    println(decimalToOtherSystem(16, 2) == List(1, 0, 0, 0, 0))
    println(decimalToOtherSystem(3, 2) == List(1, 1))
    println(decimalToOtherSystem(127, 2) == List(1, 1, 1, 1, 1, 1, 1))
    println(decimalToOtherSystem(64, 2) == List(1, 0, 0, 0, 0, 0, 0))

    val t0 = generateBinaryTree(0)
    val t1 = generateBinaryTree(1)
    val t2 = generateBinaryTree(2)
    val t3 = generateBinaryTree(3)
    val t4 = generateBinaryTree(4)
    val t5 = generateBinaryTree(5)

    println("--------------------")
    println(t0)
    println(t1)
    println(t2)
    println(t3)
    println(t4)
    println(t5)
    println("--------------------")
    println(valuesMultiplicationInBinaryTree(t0))
    println(valuesMultiplicationInBinaryTree(t1))
    println(valuesMultiplicationInBinaryTree(t2))
    println(valuesMultiplicationInBinaryTree(t3))
    println(valuesMultiplicationInBinaryTree(t4))
    println(valuesMultiplicationInBinaryTree(t5))
    println("--------------------")

  }
}
