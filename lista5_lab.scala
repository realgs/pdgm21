// Dawid Krutul
import scala.annotation.tailrec
import scala.util.Random

object lista5_lab {

  def revList[A](list: List[A]): List[A] =
    @tailrec
    def revListHelper(list: List[A], result: List[A]): List[A] =
      if list == Nil then result
      else revListHelper(list.tail, list.head :: result)
    revListHelper(list, List())
// ************************************************************************************ //
  def changeDecIntoHex[A](number: Int): List[Int] =
    @tailrec
      def changeDecIntoHexHelper[A](insideNumber: Int, ansHex: List[Int]): List[Int] =
        if(insideNumber < 16) then insideNumber :: ansHex
        else changeDecIntoHexHelper(insideNumber / 16, insideNumber % 16 :: ansHex)
      changeDecIntoHexHelper(number, List())
// ************************************************************************************ //
  def changeDecIntoAnySystem[A](number: Int, system: Int): List[Int] =
      @tailrec
      def changeDecIntoAnySystemHelper[A](insideNumber: Int, ansSystem: List[Int]): List[Int] =
        if(insideNumber < system) then insideNumber :: ansSystem
        else changeDecIntoAnySystemHelper(insideNumber / system, insideNumber % system :: ansSystem)
      changeDecIntoAnySystemHelper(number, List())
// ************************************************************************************************************ //
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](currNode: A, leftChild: BT[A], rightChild: BT[A]) extends BT[A]

  def createNDepthBinaryTree(depth: Int): BT[Double] =
    val randomVal = new scala.util.Random()
    if(depth == 0) then Empty
    else Node(randomVal.nextDouble() % 1.0, createNDepthBinaryTree(depth - 1), createNDepthBinaryTree(depth - 1))
  // ********************************************************************************************************************************* //
  def mulAllNodesInBinaryTree[A](binaryTree: BT[Double]): Double =
    binaryTree match
      case Empty => 1
      case Node(currNode, leftChild, rightChild) => currNode * mulAllNodesInBinaryTree(leftChild) * mulAllNodesInBinaryTree(rightChild)
  // ********************************************************************************************************************************* //

  // ********************************************************************************************************************************* //
  def main(args: Array[String]) = {
    // Zad 1
    println(changeDecIntoHex(31) == List(1,15))
    println(changeDecIntoHex(256) == List(1,0,0))

    // Zad 2
    println(changeDecIntoAnySystem(31,16) == List(1,15))
    println(changeDecIntoAnySystem(7,2) == List(1,1,1))

    // Zad 3
    println(createNDepthBinaryTree(0) == Empty)
    println(createNDepthBinaryTree(2) match
      case Node(currNode, Node(leftChild: Double, Empty, Empty), Node(rightChild: Double, Empty, Empty)) => true
      case _ => false)

    // Zad 4
    println(mulAllNodesInBinaryTree(Empty) == 1)
    println(mulAllNodesInBinaryTree(Node(0.1,Node(0.2,Empty,Empty),Node(0.3,Empty,Empty))) == 0.006000000000000001)
  }
}
