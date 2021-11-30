import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object Lab5 {

  def convertToHexadecimal (n : Int) : List[Int] =
    @tailrec
    def convertToHelper(number : Int, list : List[Int]) : List[Int] =
      if number==0 then list
      else convertToHelper( number/16, (number%16)::list)

    if(n >= 0) convertToHelper(n, Nil)
    else -1 :: convertToHelper(-1 * n, Nil)

  def convertToAnySystem (n : Int, base : Int) : List[Int] =
    @tailrec
    def convertToHelper(number : Int, list : List[Int]) : List[Int] =
      if number==0 then list
      else convertToHelper( number/base, (number%base)::list)

    if(n >= 0) convertToHelper(n, Nil)
    else -1 :: convertToHelper(-1 * n, Nil)

  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]


  def randomTree (levels : Int) : BinaryTree[Double] =
     if (levels > 0) then Node(Random.nextDouble(), randomTree(levels - 1), randomTree(levels - 1))
     else Empty

  def multiplyTree (tree:  BinaryTree[Double]) : Double =
     tree match
       case Empty => 1
       case Node(v, l, r) => v * multiplyTree(l) * multiplyTree(r)


  val valueOfDuplicates = 0
  def removeDuplicatesDFS (tree: BinaryTree[Double]) : BinaryTree[Double]=
    @tailrec
    def helper(queue: List[(BinaryTree[Double], Int)], visited: List[Double], result: List[(Double, Int)]): List[(Double, Int)] =
      queue match
        case Nil => result
        case (Node(v, l, r), id) :: t => if visited.contains(v) then helper(List((l, id * 2), (r, id * 2 + 1)) ::: t, visited, (valueOfDuplicates, id) :: result)
                                         else helper(List((l, id * 2), (r, id * 2 + 1)) ::: t, v :: visited, (v, id) :: result)
        case (Empty, id) :: t => helper(t, visited, result)

    makeTreeFromList(helper(List((tree, 1)), Nil, Nil), 1)




  def removeDuplicatesBFS(tree: BinaryTree[Double]) : BinaryTree[Double] =
    @tailrec
    def helper(queue: List[(BinaryTree[Double], Int)], visited: List[Double], result: List[(Double, Int)]): List[(Double, Int)] =
      queue match
        case Nil => result
        case (Node(v, l, r), id) :: t => if visited.contains(v) then helper(t ::: List((l, id * 2), (r, id * 2 + 1)), visited, (valueOfDuplicates, id) :: result)
                                         else helper(List((l, id * 2), (r, id * 2 + 1)) ::: t, v :: visited, (v, id) :: result)
        case (Empty, id) :: t => helper(t, visited, result)
    makeTreeFromList(helper(List((tree, 1)), Nil, Nil),1)


  def findNode(listOfNodes : List[(Double, Int)],  index : Int) : Double =
     listOfNodes match
       case Nil => -1
       case (value, id) :: t => if(id == index) then value
                            else findNode(t, index)

  def makeTreeFromList(listOfNodes : List[(Double, Int)], index : Int) : BinaryTree[Double] =
    val value = findNode(listOfNodes, index)
    if value == -1 then Empty
    else Node(value, makeTreeFromList(listOfNodes, 2 * index), makeTreeFromList(listOfNodes, 2 * index +1))


  def main(args: Array[String]): Unit = {

    println(convertToHexadecimal(30))
    println(convertToHexadecimal(32))
    println(convertToHexadecimal(-32))
    println()

    println(convertToAnySystem(12,2))
    println(convertToAnySystem(19,8))
    println(convertToAnySystem(-19,8))
    println()

    val tree = randomTree(3)
    println(tree)
    println()

    val tree2 = randomTree(5)
    println(tree2)
    println()

    val binaryTree = Node(1.0,Node(1.0,Node(2.0,Empty,Node(6.0, Empty, Empty)),Node(3.0,Empty,Empty)),Node(4.0,Node(5.0,Empty,Empty),Node(5.0,Empty,Empty)))
    val binaryTree2 = Node(1.0,Node(1.0,Node(1.0,Empty,Empty),Node(1.0,Empty,Empty)),Node(1.0,Node(1.0,Empty,Empty),Node(1.0,Empty,Empty)))
    val binaryTree3 = Node(1.0,Node(1.0,Node(2.0,Node(1.0,Empty,Empty),Empty),Node(3.0,Empty,Empty)),Node(4.0,Node(5.0,Empty,Node(1.0,Empty,Empty)),Node(5.0,Empty,Empty)))

    println(multiplyTree(binaryTree))
    println(multiplyTree(binaryTree2))
    println(multiplyTree(binaryTree3))
    println(binaryTree)
    println(removeDuplicatesDFS(binaryTree))
    println()
    println(binaryTree2)
    println(removeDuplicatesDFS(binaryTree2))
    println()
    println(binaryTree3)
    println(removeDuplicatesDFS(binaryTree3))
    println()
    println()

    println(binaryTree)
    println(removeDuplicatesBFS(binaryTree))
    println()
    println(binaryTree2)
    println(removeDuplicatesBFS(binaryTree2))
    println()
    println(binaryTree3)
    println(removeDuplicatesBFS(binaryTree3))
    println()
    println()
  }
}


