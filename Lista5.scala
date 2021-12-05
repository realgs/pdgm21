import scala.util.Random

object Lista5 {

  //zadanie 1

  def decimalToHexadecimal(number: Int) =
    def helper(number: Int, result: List[Int]): List[Int] =
      if number == 0
        then result
        else helper(number / 16, number % 16 :: result)
    number match
      case 0 => List(0)
      case x if x < 0 => -1 :: helper(-1 * x, Nil)
      case x => helper(x, Nil)

  def decimalToAnySystem(number: Int, systemBasis: Int) =
    def helper(number: Int, result: List[Int]): List[Int] =
      if number == 0
        then result
        else helper(number / systemBasis, number % systemBasis :: result)
    number match
      case 0 => List(0)
      case x if x < 0 => -1 :: helper(-1 * x, Nil)
      case x => helper(x, Nil)

  //drzewo

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //zadanie 3

  val random = Random
  def generateBTree(depth: Int) =
    def helper(depth: Int): BT[Double] =
      depth match
        case 1 => Node[Double](random.nextDouble(), Empty, Empty)
        case _ => Node[Double](random.nextDouble(), helper(depth - 1), helper(depth - 1))
    if depth <= 0 then Empty else helper(depth)

  //zadanie 4

  def multiplyAllBTreeNodes(bTree: BT[Double]) =
    def helper (mul: Double, stack: List[BT[Double]]): Double =
      stack match
        case Nil => mul
        case Empty::t => helper(mul, t)
        case Node(v, left, right)::t => helper(mul * v, left::right::t)
    if bTree == Empty then 0 else helper(1, List(bTree))

  //zadanie 5

  def depthFirstSearch[A](bTree: BT[A]) =
    def helper(result: List[A], stack: List[BT[A]]): List[A] =
      stack match
        case Nil => result.reverse
        case Empty::t => helper(result, t)
        case Node(v, left, right)::t => helper(v::result, left::right::t)
    helper(Nil, List(bTree))

  def breadthFirstSearch[A](bTree: BT[A]) =
    def helper(result: List[A], queue: List[BT[A]]): List[A] =
      queue match
        case Nil => result.reverse
        case Empty::t => helper(result, t)
        case Node(v, left, right)::t => helper(v::result, t:::List(left, right))
    helper(Nil, List(bTree))

  def noDuplicatesDFS[A](bTree:BT[A], defaultValue: A) =
    def helper(stack: List[(BT[A], Int)], visited: List[A], noDuplicates: List[(A, Int)]): List[(A, Int)] =
      stack match
        case Nil => noDuplicates.reverse
        case (Empty, _)::t => helper(t, visited, noDuplicates)
        case (Node(v, left, right), index)::t => if visited.contains(v) then helper((left, index * 2)::(right, index * 2 + 1)::t, visited, (defaultValue, index)::noDuplicates)
                                                    else helper((left, index * 2)::(right, index * 2 + 1)::t, v::visited, (v, index)::noDuplicates)
    bTreeFromList(helper(List((bTree,1)),Nil,Nil), defaultValue)

  def noDuplicatesBFS[A](bTree:BT[A], defaultValue: A) =
    def helper(queue: List[(BT[A], Int)], visited: List[A], noDuplicates: List[(A, Int)]): List[(A, Int)] =
      queue match
        case Nil => noDuplicates.reverse
        case (Empty, _)::t => helper(t, visited, noDuplicates)
        case (Node(v, left, right), index)::t => if visited.contains(v) then helper(t:::List((left, index * 2),(right, index * 2 + 1)), visited, (defaultValue, index)::noDuplicates)
                                                    else helper(t:::List((left, index * 2),(right, index * 2 + 1)), v::visited, (v, index)::noDuplicates)
    bTreeFromList(helper(List((bTree,1)),Nil,Nil), defaultValue)

  def bTreeFromList[A](nodesList: List[(A, Int)],defaultValue: A) =
    def findNode(currentNodeList: List[(A, Int)], index:Int): BT[A] =
      currentNodeList match
        case Nil => Empty
        case (v, id)::t => if id == index then Node(v, findNode(nodesList, index * 2), findNode(nodesList, index * 2 + 1)) else findNode(t, index)
    findNode(nodesList, 1)

  def main(args: Array[String]): Unit = {

    //zadanie 1

    println(decimalToHexadecimal(31))
    println(decimalToHexadecimal(16))
    println(decimalToHexadecimal(244))
    println(decimalToHexadecimal(-31))
    println(decimalToHexadecimal(0))
    println()

    println(decimalToAnySystem(31, 16))
    println(decimalToAnySystem(31, 8))
    println(decimalToAnySystem(31, 2))
    println(decimalToAnySystem(-31, 2))
    println(decimalToAnySystem(0, 2))
    println()

    //BTree

    val tree1 = generateBTree(3)
    println(multiplyAllBTreeNodes(tree1))
    println(breadthFirstSearch(tree1))
    println(depthFirstSearch(tree1))
    println()

    val tree2 = Node(1,Node(2,Node(3,Empty,Empty),Node(4,Empty,Empty)),Node(3,Node(5,Empty,Empty),Node(6,Empty,Empty)))
    println(breadthFirstSearch(tree2))
    println(depthFirstSearch(tree2))
    println(noDuplicatesBFS(tree2, null))
    println(noDuplicatesDFS(tree2, null))
  }
}
