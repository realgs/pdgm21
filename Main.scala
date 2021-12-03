import java.util.Random
import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit =

    //task 1 - test
    println("task 1")
    println(convertDecimalToListOfHexDigits(-23) == List(15, 15, 15, 15, 15, 15, 14, 9))
    println(convertDecimalToListOfHexDigits(-12873) == List(15, 15, 15, 15, 12, 13, 11, 7))
    println(convertDecimalToListOfHexDigits(0) == List(0))
    println(convertDecimalToListOfHexDigits(31) == List(1, 15))
    println(convertDecimalToListOfHexDigits(3456) == List(13, 8, 0))

    //convertNumberToListOfDigits(0, 23) //throws exception
    println(convertDecimalToListOfDigits(251, 2) == List(1, 1, 1, 1, 1, 0, 1, 1))
    println(convertDecimalToListOfDigits(251, 8) == List(3, 7, 3))
    println(convertDecimalToListOfDigits(251, 16) == List(15, 11))
    println(convertDecimalToListOfDigits(251, 20) == List(12, 11))
    println()

    //task 3 - test
    println("task 3")
    //generateTree(-1) //throws exception
    println(generateTree(0) == Empty)
    println(generateTree(1))
    println(generateTree(3))
    println()

    //task 4 - test
    println("task 4")
    println(multiplyTreeNodes(generateTree(5)))
    println(multiplyTreeNodes(generateTree(16)))
    println(multiplyTreeNodes(generateTree(3)))
    println()

    //task 5 - test
    println("task 5")
    val tree = Node(2, Node(5, Node(4, Node(1, Empty, Empty), Node(8, Empty, Empty)), Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))), Node(4, Node(6, Node(0, Empty, Empty), Node(-1, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(8, Empty, Empty))))
    println(deleteNodeDepth(Empty) == Nil)
    println(deleteNodeDepth(Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty))) == List(1))
    println(deleteNodeDepth(tree) == List(2, 5, 4, 1, 8, 3, 6, 0, -1))

    println(deleteNodeBreadth(Empty) == Nil)
    println(deleteNodeBreadth(Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty))) == List(1))
    println(deleteNodeBreadth(tree) == List(2, 5, 4, 1, 6, 3, 8, 0, -1))

  //task 1
  def convertDecimalToListOfHexDigits(decimal: Int): List[Int] =

    @tailrec
    def convertPositive(num: Int, digitsList: List[Int]): List[Int] =
      if num < 16 then num :: digitsList
      else convertPositive(num / 16, (num % 16) :: digitsList)

    //int has 32 bits(4 bytes)
    val hexNr = 8 //number of hexadecimals
    @tailrec
    def convertNegative(counter: Int, num: Int, digitsList: List[Int]): List[Int] = //function uses two's complement
      if counter == hexNr then digitsList
      else if counter < hexNr && num == 0 then convertNegative(counter + 1, 0, 15 :: digitsList)
      else {

        val reminder = -(num % 16)
        if counter != 0 then convertNegative(counter + 1, num / 16, (15 - reminder) :: digitsList)
        else convertNegative(counter + 1, num / 16, (15 - reminder + 1) :: digitsList)
      }

    if decimal < 0 then convertNegative(0, decimal, List())
    else convertPositive(decimal, List())

  def convertDecimalToListOfDigits(decimal: Int, base: Int): List[Int] =

    @tailrec
    def convert(num: Int, digitsList: List[Int]): List[Int] =
      if num < base then num :: digitsList
      else convert(num / base, (num % base) :: digitsList)

    if decimal < 0 || base <= 0 then throw new Exception("Invalid arguments: negative value or base equals 0")
    else convert(decimal, List())


  //task 3
  sealed trait BinaryTree[+T]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+T](elem: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

  val rand = new Random()
  def generateTree(levelsNr: Int): BinaryTree[Double] =
    if levelsNr < 0 then throw new Exception("Invalid input: negative tree depth")

    levelsNr match
      case 0 => Empty
      case _ => Node(rand.nextDouble(), generateTree(levelsNr - 1), generateTree(levelsNr - 1))


  //task 4
  /*
  def multiplyTreeNodes(tree: BinaryTree[Double]): Double =
    tree match
      case Empty => 1.0
      case Node(value, leftTree, rightTree) => value * multiplyTreeNodes(leftTree) * multiplyTreeNodes(rightTree)
  */
  def multiplyTreeNodes(tree: BinaryTree[Double]): Double =

    def multiply(treeNotVisited: BinaryTree[Double], result: Double): Double =
      treeNotVisited match
        case Empty => result
        case Node(value, leftTree, rightTree) => multiply(leftTree, multiply(rightTree, result * value)) //reduces number of stack frames

    if tree == Empty then throw new Exception("Empty tree")
    else multiply(tree, 1.0)


  //task 5
  @tailrec
  def searchList[T](list: List[T], elem: T): Boolean =
    list match
      case Nil => false
      case head::tail => elem == head || searchList(tail, elem)

  def reverseList[T](list: List[T]): List[T] =

    @tailrec
    def reverse[T](list: List[T], revList: List[T]): List[T] =
      list match
        case Nil => revList
        case head::tail => reverse(tail, head :: revList)
    reverse(list, List())

  //def convertListToTree[T](list: List[T]): BinaryTree[T] =

  def deleteNodeBreadth[T](tree: BinaryTree[T]): List[T] =

    @tailrec
    def breadth[T](nodeQueue: List[BinaryTree[T]], visited: List[T]): List[T] =
      nodeQueue match
        case Nil => reverseList(visited)
        case Empty :: tail => breadth(tail, visited)
        case Node(value, leftTree, rightTree) :: tail =>
          if !searchList(visited, value) then breadth(tail ++ List(leftTree, rightTree), value :: visited)
          else breadth(tail ++ List(leftTree, rightTree), visited)

    breadth(List(tree), List())

  def deleteNodeDepth[T](tree: BinaryTree[T]): List[T] =

    def depth[T](tree: BinaryTree[T], visited: List[T]): List[T] =
      tree match
        case Empty => visited
        case Node(value, leftTree, rightTree) =>
          if !searchList(visited, value) then depth(rightTree, depth(leftTree, value :: visited))
          else depth(rightTree, depth(leftTree, visited))

    reverseList(depth(tree, List()))
}

