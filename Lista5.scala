object Lista5 {
//  zadanie 1
  def decToHex(number: Int): List[Int] =
    def decToHexIter(number: Int, result: List[Int]): List[Int] =
      number match
        case 0 => result
        case _ => decToHexIter(number / 16, number % 16 :: result)
    if number == 0 then List(0)
    else if number < 0 then Nil
    else decToHexIter(number, Nil)

//  zadanie 2
  def decToAny(number: Int, system: Int): List[Int] =
    def decToAnyIter(number: Int, result: List[Int]): List[Int] =
      number match
        case 0 => result
        case _ => decToAnyIter(number / system, number % system :: result)
    if system <= 1 then Nil
    else if  number == 0 then 0 :: Nil
    else if number < 0 then Nil
    else decToAnyIter(number, Nil)


  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = scala.util.Random

//  zadanie 3
  def generateTree(depth: Int): BT[Double] =
    depth match
      case 0 => Empty
      case _ => Node(random.nextDouble(), generateTree(depth-1), generateTree(depth-1))

//  zadanie 4
  def calculateProductOfNodes(node: BT[Double]): Double =
    node match
      case Empty => 1
      case Node(value, left, right) => value * calculateProductOfNodes(left) * calculateProductOfNodes(right)

  def depthBT[A](tree: BT[A]): List[A] =
    def traverse[A](stack: List[BT[A]]): List[A] =
      stack match
        case Nil => Nil
        case Empty :: tail => traverse(tail)
        case Node(value, left ,right) :: tail => value :: traverse(left :: right :: tail)
    traverse(List(tree))

  def breadthBT[A](tree: BT[A]): List[A] =
    def traverse[A](queue: List[BT[A]]): List[A] =
      queue match
        case Nil => Nil
        case Empty :: tail => traverse(tail)
        case Node(value, left ,right) :: tail => value :: traverse(tail ::: List(left, right))
    traverse(List(tree))

//  zadanie 5
  def removeDuplicatesDFS[A](tree: BT[A]): BT[A] =
    def DFS[A](stack: List[BT[A]], visitedNodes: List[A]): List[A] =
      stack match
        case Nil => visitedNodes.reverse
        case Empty :: tail => DFS(tail, visitedNodes)
        case Node(value, left, right) :: tail =>  if !visitedNodes.contains(value) then DFS(left :: right :: tail, value :: visitedNodes)
                                                  else DFS(left :: right :: tail, visitedNodes)
    val newTree = DFS(List(tree), Nil)
    createTreeFromList(newTree, 1, newTree.length)

  def removeDuplicatesBFS[A](tree: BT[A]): BT[A] =
    def BFS[A](stack: List[BT[A]], visitedNodes: List[A]): List[A] =
      stack match
        case Nil => visitedNodes.reverse
        case Empty :: tail => BFS(tail, visitedNodes)
        case Node(value, left, right) :: tail =>  if !visitedNodes.contains(value) then BFS(tail ::: List(left, right), value :: visitedNodes)
                                                  else BFS(tail ::: List(left, right), visitedNodes)
    val newTree = BFS(List(tree), Nil)
    createTreeFromList(newTree, 1, newTree.length)

  def findNode[A](list: List[A], index: Int): A =
    (list, index) match
      case (Nil, _) => throw new Exception("empty list")
      case (value :: _, 0) => value
      case (_ :: tail, index) => findNode(tail, index - 1)

  def createTreeFromList[A](list: List[A], index: Int, treeSize: Int): BT[A] =
    if treeSize < index then Empty
    else Node(findNode(list, index - 1),
          createTreeFromList(list, index * 2, treeSize),
          createTreeFromList(list, index * 2 + 1, treeSize))

  def main(args: Array[String]): Unit = {
    //zadanie 1
    println("convert decimal to hex: ")
    println("31:  " + decToHex(31))
    println("123: " + decToHex(123))
    println("0:   " + decToHex(0))
    println("-31:   " + decToHex(-31))

    //zadanie 2
    println("\nconvert any to hex: ")
    println("31 to hex:     " + decToAny(31, 16))
    println("127 to binary: " + decToAny(127, 2))
    println("0 to binary:   " + decToAny(0, 2))

    //zadanie 3
    println("\ngenerate tree depth = 3: ")
    println(breadthBT(generateTree(3)))

    //zadanie 4
    println("\nproduct of generated tree depth = 3: ")
    println(calculateProductOfNodes(generateTree(3)))

    //zadanie 5
    println("\nremove duplicates from tree: ")
    val tree1 = Node(1, Node(2, Node(3, Empty, Empty), Node(3, Node(4, Node(5, Empty, Empty), Empty), Empty)), Node(5, Node(1, Empty, Empty), Empty))
    val tree2 = Node(1, Node(2, Node(3, Empty, Empty), Node(3, Node(4, Node(5, Empty, Empty), Empty), Empty)), Node(5, Node(1, Empty, Empty), Empty))

    println("bfs")
    println("before: " + breadthBT(tree1))
    println("after: " + breadthBT(removeDuplicatesBFS(tree1)))
    println("\ndfs")
    println("before: " + depthBT(tree2))
    println("after: " + depthBT(removeDuplicatesDFS(tree2)))
  }
}
