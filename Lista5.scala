//Szymon Sawczuk

object Lista5 {

//  Zadanie 1 i 2
  def convertToHex(number:Int) =
    def convert(number:Int, isNegative:Boolean, result:List[Int]):List[Int] =
      if number < 16 then
        if isNegative then -1::number::result else number::result
      else convert(number/16, isNegative, number%16 :: result)
    convert(Math.abs(number), number < 0, Nil)

  def convertToAny(number:Int, base:Int) =
    def convert(number:Int, isNegative:Boolean,result:List[Int]):List[Int] =
      if number < base then
        if isNegative then -1::number::result else number::result
      else convert(number/base, isNegative, number%base :: result)
    convert(Math.abs(number), number < 0, Nil)

//  Zadanie 3
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[+A](elem:A, left:Tree[A], right:Tree[A]) extends Tree[A]

  val random = scala.util.Random

  def generateTree(height:Int):Tree[Double] =
    if height <= 0 then Empty
    else Node(random.nextDouble(), generateTree(height-1), generateTree(height-1))



//  Zadanie 4
  def productOfTree(tree:Tree[Double]) =
    def calculate(stack:List[Tree[Double]], result:Double):Double =
      stack match
        case Nil => result
        case Empty::tail => calculate(tail, result)
        case Node(element, left, right)::tail => calculate(left::right::tail, element * result)
    if tree == Empty then 0 else calculate(tree::Nil, 1)

//  Helper print

  def breadthBT[A](tree: Tree[A]) =
    def breadth(queue:List[(Tree[A], Int)],  result:List[(A, Int)]):List[(A, Int)] =
      queue match
        case Nil => result.reverse
        case (Empty,height)::tail => breadth(tail, result)
        case (Node(element, left, right), height)::tail => breadth(tail:::(left, height * 2  + 1)::(right, height * 2 + 2)::Nil, (element, height)::result)
    breadth((tree,0)::Nil, Nil)

  def printTree[A](tree: Tree[A]) =
    def printT(currentHeight:Double, currentNumber:Int, list:List[(A, Int)]): Unit =
      if list == Nil then println()
      else
        if currentNumber == ( Math.pow(2, currentHeight ) - 1 ) then
          println()
          printT(currentHeight + 1, currentNumber, list)
        else
          if list.head._2 == currentNumber then
            print(list.head._1)
            print(" ")
            printT(currentHeight, currentNumber + 1, list.tail)
          else
            print("  ")
            printT(currentHeight, currentNumber + 1, list)
    printT(1,0, breadthBT(tree))

//  Zadanie 5
  def DFSDuplicatesRemove[A](tree: Tree[A]) =
    def DFS(stack:List[Tree[A]], visited:List[A]):List[A] =
      stack match
        case Nil => visited.reverse
        case Empty::tail => DFS(tail, visited)
        case Node(element, left, right)::tail =>
          if !visited.contains(element) then DFS(left::right::tail, element::visited)
          else DFS(left::right::tail, visited)
    DFS(tree::Nil, Nil)

  def BFSDuplicatesRemove[A](tree: Tree[A]) =
    def BFS(queue:List[Tree[A]], visited:List[A]):List[A] =
      queue match
        case Nil => visited.reverse
        case Empty::tail => BFS(tail, visited)
        case Node(element, left, right)::tail =>
          if !visited.contains(element) then BFS(tail:::left::right::Nil, element::visited)
          else BFS(tail:::left::right::Nil, visited)
    BFS(tree::Nil, Nil)

  import scala.math.Ordering
  import Ordering.Implicits._

  def createWithoutDuplicates[A: Ordering](list:List[A]):Tree[A] =
    if list == Nil then Empty
    else Node(list.head, createWithoutDuplicates(list.filter(_ < list.head)), createWithoutDuplicates(list.filter(_ > list.head)))

  def removeDuplicatesDFS[A: Ordering](tree:Tree[A]):Tree[A] = createWithoutDuplicates(DFSDuplicatesRemove(tree))

  def removeDuplicatesDFS(tree:Tree[Nothing]):Tree[Nothing] = Empty

  def removeDuplicatesBFS[A: Ordering](tree:Tree[A]):Tree[A] = createWithoutDuplicates(BFSDuplicatesRemove(tree))

  def removeDuplicatesBFS(tree:Tree[Nothing]):Tree[Nothing] = Empty


  def main(args: Array[String]): Unit = {
    println("ConvertToHex")
    println(convertToHex(31))
    println(convertToHex(-31))
    println(convertToHex(128))
    println(convertToHex(64))
    println(convertToHex(0))
    println(convertToHex(3))
    println(convertToHex(16))
    println(convertToHex(-16))

    println("ConvertToAny")

    println(convertToAny(31, 2))
    println(convertToAny(-31, 2))
    println(convertToAny(128, 3))
    println(convertToAny(-128, 3))
    println(convertToAny(64, 10))
    println(convertToAny(-64, 10))
    println(convertToAny(17, 2))
    println(convertToAny(-17, 2))
    println(convertToAny(9, 8))
    println(convertToAny(-9, 8))

    println("GenerateTree")

    val tree = generateTree(3)
    println(tree)
    printTree(tree)
    println()
    printTree(generateTree(5))
    println()
    printTree(generateTree(0))
    println()
    printTree(generateTree(1))

    println("Product")

    println(productOfTree(tree))
    println(productOfTree(generateTree(0)))
    println(productOfTree(generateTree(1)))

    println("RemoveDuplicatesDFS")

    val BSTTree = Node(8, Node(6, Node(2, Node(1,Empty,Empty),Empty),Node(7, Node(7,Empty,Empty),Node(8,Empty,Empty))), Node(9, Node(9, Empty, Empty),Node(10,Empty,Empty)))
    val BSTTree2 = Node(7,Node(5,Node(4,Empty,Empty),Node(7,Node(6,Empty,Empty),Empty)),Node(9,Node(9,Empty,Empty),Node(10,Node(10,Empty,Empty),Empty)))
    val BSTTree3 = Node(10,Node(4,Node(4,Node(2,Node(1,Empty,Empty),Node(3,Empty,Empty)),Empty),Node(9,Node(8,Empty,Empty),Node(10,Empty,Empty))),Node(12,Node(11,Empty,Node(12,Empty,Empty)),Node(13,Empty,Empty)))
    printTree(BSTTree)
    println()
    printTree(removeDuplicatesDFS(BSTTree))
    println()
    printTree(removeDuplicatesDFS(Empty))
    println()
    printTree(BSTTree2)
    println()
    printTree(removeDuplicatesDFS(BSTTree2))
    println()
    printTree(BSTTree3)
    println()
    printTree(removeDuplicatesDFS(BSTTree3))

    println("RemoveDuplicatesBFS")

    printTree(BSTTree)
    println()
    printTree(removeDuplicatesBFS(BSTTree))
    println()
    printTree(removeDuplicatesBFS(Empty))
    println()
    printTree(BSTTree2)
    println()
    printTree(removeDuplicatesBFS(BSTTree2))
    println()
    printTree(BSTTree3)
    println()
    printTree(removeDuplicatesBFS(BSTTree3))

  }


}
