import scala.annotation.tailrec
import scala.util.Random

object Main {

  def reverseList[A](list: List[A]): List[A] = {
    @tailrec
    def reverseHelper(list: List[A], result: List[A]): List[A] = {
      if list.isEmpty then result
      else reverseHelper(list.tail, list.head :: result)
    }
    reverseHelper(list, List())
  }


  def convertToHex(number: Int) = {
    def convertToHexHelper(number: Int, result_hex: List[Int]): List[Int] = {
      if number == 0 then result_hex
      else convertToHexHelper(number / 16, number % 16 :: result_hex)
    }
    if number == 0 then List(0)
    else if number < 0 then Nil
    else convertToHexHelper(number, Nil);
  }

  def convertToAnySystem(numberSystemList: List[Int]) = {
    def convertToAnySystemHelper(number: Int, system: Int, result: List[Int]): List[Int] = {
      if number == 0 then result
      else convertToAnySystemHelper(number / system, system, number % system :: result)
    }

    if numberSystemList(0) == 0 then List(0)
    else if (numberSystemList(0) < 0 || numberSystemList(1) <= 0) then Nil
    else convertToAnySystemHelper(numberSystemList(0), numberSystemList(1), Nil)
  }




  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  // Zadanie 3
  def generateTree(depth: Int): BT[Double] = {
    if depth == 0 then Empty
    else Node(scala.util.Random.nextDouble(), generateTree(depth - 1), generateTree(depth - 1))
  }

  // Zadanie 4
  def multiplyNodes(startNode: BT[Double]): Double = {
    startNode match
        case Empty => 1
        case Node(value, leftNode, rightNode) => value * multiplyNodes(leftNode)
                                                       * multiplyNodes(rightNode)
  }

  // Zadanie 5
  def createTreeFromList[A](nodes : List[A], length : Int) : BT[A] = {
    nodes match
      case h :: t =>
        val (left, right) = t.splitAt(length)
        Node(h, createTreeFromList(left, length / 2), createTreeFromList(right, length / 2))
      case _ => Empty
  }

  def listLength[A](list :List[A]): Int = {
    if list == Nil then 0
    else 1 + listLength(list.tail)
  }

  def removeDuplicatesDFS[A](bt: BT[A]) =
    def DFS(nodes_stack: List[BT[A]], uniqueValues: List[A]): List[A] =
      nodes_stack match
        case Nil => reverseList(uniqueValues)
        case Empty :: t => DFS(t, uniqueValues)
        case Node(v, l, r) :: t =>
          if uniqueValues.contains(v) then DFS(l :: r :: t, uniqueValues)
          else DFS(l :: r :: t, v :: uniqueValues)

    val uniqueList = DFS(bt :: Nil, Nil)
    createTreeFromList(uniqueList, listLength(uniqueList))

  def removeDuplicatesBFS[A](bt: BT[A]) =
    def BFS(nodes_queue: List[BT[A]], uniqueValues: List[A]): List[A] =
      nodes_queue match
        case Nil => reverseList(uniqueValues)
        case Empty :: t => BFS(t, uniqueValues)
        case Node(v, l, r) :: t =>
          if uniqueValues.contains(v) then BFS(t ++ (l :: r :: Nil), uniqueValues)
          else BFS(t ++ (l :: r :: Nil), v :: uniqueValues)

    val uniqueList = BFS(bt :: Nil, Nil)
    createTreeFromList(uniqueList, listLength(uniqueList))


  def main(args: Array[String]): Unit = {
    println(convertToHex(31) == List(1, 15))
    println(convertToHex(196) == List(12, 4))
    println(convertToHex(0) == List(0))
    println()

    println(convertToAnySystem(List(31, 16)) == List(1, 15))
    println(convertToAnySystem(List(196, 2)) == List(1, 1, 0, 0, 0, 1, 0, 0))
    println(convertToHex(0) == List(0))
    println()

    val tree1 = generateTree(1)
    val tree2 = generateTree(2)
    val tree3 = generateTree(5)
    val tree4 = generateTree(0)

    println(tree1)
    println(tree2)
    println(tree3)
    println(tree4)
    println()

    println(multiplyNodes(tree1))
    println(multiplyNodes(tree2))
    println(multiplyNodes(tree3))
    println(multiplyNodes(tree4))
    println()

    val tree5 = Node(1,
                       Node(2,
                          Node(3,
                            Node(4,
                              Node(8,
                                Node(3, Empty, Empty),
                                Node(9, Empty, Empty)
                                   ),
                            Node(14, Empty, Empty)
                            ),
                          Node(16, Empty, Empty)
                          ),
                       Node(9, Empty, Empty)
                       ),
                   Node(3, Node(3, Empty, Empty), Empty))


    val tree6 = Node(0,
                     Node(0, Empty, Empty),
                     Empty)

    val tree7 = Empty


    println(removeDuplicatesDFS(tree5))
    println(removeDuplicatesDFS(tree6))
    println(removeDuplicatesDFS(tree7))
    println()

    println(removeDuplicatesBFS(tree5))
    println(removeDuplicatesBFS(tree6))
    println(removeDuplicatesBFS(tree7))
  }
}