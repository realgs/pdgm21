import scala.util.Random

object Main {

/*
  def findNumberList(number: Int): List[Any] = {
    def findList(number: Int, numberList: List[Any]): List[Any] = {
      number match
        case 0 => numberList
        case _ => number % 16 match
          case 10 => findList(number/16, "A" :: numberList)
          case 11 => findList(number/16, "B" :: numberList)
          case 12 => findList(number/16, "C" :: numberList)
          case 13 => findList(number/16, "D" :: numberList)
          case 14 => findList(number/16, "E" :: numberList)
          case 15 => findList(number/16, "F" :: numberList)
          case _ => findList(number/16, java.lang.Math.floorMod(number, 16) :: numberList)
    }
    findList(number, List())
  }
*/

  //zadanie 1

  def findNumber16(number: Int): List[Any] = {
    def findList(number: Int, numberList: List[Any]): List[Any] =
      number match
        case 0 => numberList
        case _ => findList(number/16, (number % 16) :: numberList)
    findList(number/16, (number % 16) :: List())
  }

  //zadanie 2

  def findNumberAnySystem(number: Int, system: Int): List[Any] = {
    def findList(number: Int, numberList: List[Any]): List[Any] =
      number match
        case 0 => numberList
        case _ => findList(number/system, (number%system) :: numberList)
    findList(number/system, (number%system) :: List())
  }

  // zadanie 3

  sealed trait BinaryTree[+A]
    case object Empty extends BinaryTree[Nothing]
    case class Node[+A](element: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  val r = scala.util.Random

  def generateBinaryTree(depth : Int): BinaryTree[Double] = {
    depth match
      case 0 => Empty
      case _ => Node(r.nextFloat, generateBinaryTree(depth - 1), generateBinaryTree(depth - 1))
  }

  // zadanie 4

  def treeElementsMultiplication(tree: BinaryTree[Double]): Double = {
    def treeElementsMultiplicationInner(toVisit: BinaryTree[Double], result: Double): Double =
      toVisit match
        case Empty => result
        case Node(value, leftSubtree, rightSubtree) => treeElementsMultiplicationInner(leftSubtree, treeElementsMultiplicationInner(rightSubtree, value * result))
    treeElementsMultiplicationInner(tree, 1.0)
  }

  /*
  def treeElementsMultiplication(tree: BinaryTree[Double]): Double = {
    tree match
      case Node(value, Empty, Empty) => value
      case Node(value, leftSubtree, rightSubtree) => value * treeElementsMultiplication2(leftSubtree) * treeElementsMultiplication2(rightSubtree)
      case Empty => 1.0
  }
  */

  // zadanie 5

  def deleteDuplicateDfs(tree: BinaryTree[Double]): BinaryTree[Double]  = {
    def deleteDuplicateInner(toVisit: List[(Double, Int)], visited: List[(Double, Int)], nodeValues: List[Double], visitedNodesSum: Double): List[(Double, Int)] =
      toVisit match
        case Nil => visited
        case (head, id) :: tail => if nodeValues contains head then deleteDuplicateInner(tail, (visitedNodesSum, id) :: visited, visitedNodesSum :: nodeValues, visitedNodesSum + head) else deleteDuplicateInner(tail, (head, id) :: visited, head :: nodeValues, visitedNodesSum + head)
    buildTreeFromList(deleteDuplicateInner(dfs(tree), List(), List(),0), 1)
  }

  def dfs(tree: BinaryTree[Double]): List[(Double, Int)]  = {
    def dfsInner(toVisit: BinaryTree[Double], visited: List[(Double, Int)], id: Int): List[(Double, Int)] =
      toVisit match
        case Empty => visited
        case Node(value, leftSubtree, rightSubtree) => dfsInner(rightSubtree, dfsInner(leftSubtree, (value, id) :: visited, id*2), 2*id + 1)
    dfsInner(tree, Nil, 1)
  }



  def deleteDuplicateBfs(tree: BinaryTree[Double]): BinaryTree[Double]  = {
    def deleteDuplicateInner(toVisit: List[(Double, Int)], visited: List[(Double, Int)], nodeValues: List[Double], visitedNodesSum: Double): List[(Double, Int)] =
      toVisit match
        case Nil => visited
        case (head, id) :: tail => if nodeValues contains head then deleteDuplicateInner(tail, (visitedNodesSum, id) :: visited, visitedNodesSum :: nodeValues, visitedNodesSum + head) else deleteDuplicateInner(tail, (head, id) :: visited, head :: nodeValues, visitedNodesSum + head)
    buildTreeFromList(deleteDuplicateInner(bfs(tree), List(), List(), 0), 1)
  }

  def bfs(tree: BinaryTree[Double]): List[(Double, Int)] = {
    def bfsInner(treeNodesList: List[BinaryTree[Double]], visited: List[(Double, Int)], id: Int): List[(Double, Int)] =
      treeNodesList match
        case Nil => visited
        case Empty :: tail => bfsInner(tail, visited, id + 1)
        case Node(value, leftSubtree, rightSubtree) :: tail => bfsInner(tail ::: List(leftSubtree, rightSubtree), (value, id) :: visited, id + 1)
    reverseList(bfsInner(List(tree), Nil, 1))
  }

  /*
  def bfs(tree: BinaryTree[Double]): List[(Double, Int)] = {
    def bfsInner(treeNodesList: List[BinaryTree[Double]], id: Int): List[(Double, Int)] =
      treeNodesList match
        case Nil => Nil
        case Empty :: tail => bfsInner(tail, id + 1)
        case Node(value, leftSubtree, rightSubtree) :: tail => (value, id) :: bfsInner(tail ::: List(leftSubtree, rightSubtree), id + 1)
    bfsInner(List(tree), 1)
  }
*/



  def buildTreeFromList(nodeList: List[(Double, Int)], index: Int): BinaryTree[Double] = {
    findNextNode(nodeList, (value, id) => id == index) match
      case -1 => Empty
      case value => Node(value, buildTreeFromList(nodeList, index*2), buildTreeFromList(nodeList, index*2 + 1))
  }

  def findNextNode(nodeList: List[(Double, Int)], predicate: (Double, Int) => Boolean): Double = {
    nodeList match
      case Nil => -1
      case (value, id) :: tail => if predicate(value, id) then value else findNextNode(tail, predicate)
  }

  def reverseList[A](list: List[A]): List[A] = {
    def reverseListInner(newList: List[A], oldList: List[A]): List[A] =
      oldList match
        case Nil => newList
        case head :: tail => reverseListInner(head :: newList, tail)
    reverseListInner(Nil, list)
  }


  def main(args: Array[String]): Unit = {
    println("test 1")
    println(findNumber16(31) == List(1, 15))
    println(findNumber16(5) == List(5))
    println(findNumber16(50) == List(3, 2))
    println(findNumber16(0) == List(0))

    println("\ntest 2")
    println(findNumberAnySystem(0, 5) == List(0))
    println(findNumberAnySystem(31, 16) == List(1, 15))
    println(findNumberAnySystem(9, 2) == List(1, 0, 0, 1))
    println(findNumberAnySystem(10, 3) == List(1, 0, 1))

    println("\ntest 3")
    println(generateBinaryTree(0))
    println(generateBinaryTree(1))
    println(generateBinaryTree(2))
    println(generateBinaryTree(3))

    println("\ntest 4")
    println(treeElementsMultiplication(Node(0.5, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty))) == 0.125)
    println(treeElementsMultiplication(Node(0.5, Node(0.5, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty)), Node(0.5, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty)))) == 0.0078125)
    println(treeElementsMultiplication(Node(0.5, Empty, Empty)) == 0.5)

    println("\ntest 5 dfs")
    println(deleteDuplicateDfs(Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(5, Node(2, Empty, Empty), Node(6, Empty, Empty)))) == Node(1.0,Node(22.0,Node(4.0,Empty,Empty),Node(13.0,Empty,Empty)),Node(5.0,Node(2.0,Empty,Empty),Node(6.0,Empty,Empty))))
    println(deleteDuplicateDfs(Node(1, Node(2, Node(4, Empty, Empty), Node(7, Empty, Empty)), Node(3, Node(2, Empty, Node(6, Empty, Empty)), Empty))) == Node(1.0,Node(22.0,Node(4.0,Empty,Empty),Node(7.0,Empty,Empty)),Node(3.0,Node(2.0,Empty,Node(6.0,Empty,Empty)),Empty)))
    println(deleteDuplicateDfs(Node(1, Node(2, Node(4, Node(5, Empty, Empty), Empty), Node(7, Empty, Empty)), Node(5, Node(2, Empty, Node(6, Empty, Empty)), Empty))) == Node(1.0,Node(29.0,Node(4.0,Node(20.0,Empty,Empty),Empty),Node(7.0,Empty,Empty)),Node(5.0,Node(2.0,Empty,Node(6.0,Empty,Empty)),Empty)))
    println(deleteDuplicateDfs(Node(1, Empty, Empty)) == Node(1.0,Empty,Empty))

    println("\ntest 5 bfs")
    println(deleteDuplicateBfs(Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(5, Node(2, Empty, Empty), Node(6, Empty, Empty)))) == Node(1.0,Node(2.0,Node(4.0,Empty,Empty),Node(12.0,Empty,Empty)),Node(5.0,Node(17.0,Empty,Empty),Node(6.0,Empty,Empty))))
    println(deleteDuplicateBfs(Node(1, Node(2, Node(4, Empty, Empty), Node(7, Empty, Empty)), Node(3, Node(2, Empty, Node(6, Empty, Empty)), Empty))) == Node(1.0,Node(2.0,Node(4.0,Empty,Empty),Node(7.0,Empty,Empty)),Node(3.0,Node(17.0,Empty,Node(6.0,Empty,Empty)),Empty)))
    println(deleteDuplicateBfs(Node(1, Node(2, Node(4, Node(5, Empty, Empty), Empty), Node(7, Empty, Empty)), Node(5, Node(2, Empty, Node(6, Empty, Empty)), Empty))) == Node(1.0,Node(2.0,Node(4.0,Node(21.0,Empty,Empty),Empty),Node(7.0,Empty,Empty)),Node(5.0,Node(19.0,Empty,Node(6.0,Empty,Empty)),Empty)))
    println(deleteDuplicateBfs(Node(1, Empty, Empty)) == Node(1.0,Empty,Empty))
  }
}
