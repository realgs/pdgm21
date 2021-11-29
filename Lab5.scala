import scala.util.Random
import scala.annotation.tailrec

object Lab5
{
  def decToHex(dec: Int): List[Int] =
    if dec < 0 then Nil else
      @tailrec
      def decToHexHelper(decInConversion: Int, result: List[Int]): List[Int] =
        decInConversion match
          case 0 => result
          case _ => decToHexHelper(decInConversion / 16, (decInConversion % 16) :: result)
      decToHexHelper(dec / 16, (dec % 16) :: Nil)

  def decToSystem(dec: Int, system: Int): List[Int] =
    if dec < 0 then Nil else
      @tailrec
      def decToSystemHelper(decInConversion: Int, result: List[Int]): List[Int] =
        decInConversion match
          case 0 => result
          case _ => decToSystemHelper(decInConversion / system, (decInConversion % system) :: result)
      decToSystemHelper(dec / system, (dec % system) :: Nil)

  //zadanie 3
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val r = scala.util.Random

  def generateTree(depth: Int): BT[Double] =
    depth match
      case 0 => Empty
      case _ => Node(r.nextFloat, generateTree(depth - 1), generateTree(depth - 1))

  //zadanie 4
  def treeProduct(tree: BT[Double]): Double =
    def treeProductHelper(nodesToVisit: BT[Double], result: Double): Double =
      nodesToVisit match
        case Empty => result
        case Node(value, leftSubtree, rightSubtree) => treeProductHelper(rightSubtree, treeProductHelper(leftSubtree, value * result))
    treeProductHelper(tree, 1.0)

  //zadanie 5
  def bfs[A] (tree: BT[A]): List[A] =
    def bfsQueue(queue: List[BT[A]]): List[A] =
      queue match
        case Nil => Nil
        case Empty :: t => bfsQueue(t)
        case Node(value, lTree, rTree) :: t => value :: bfsQueue(t ::: List(lTree, rTree))
    bfsQueue(tree :: Nil)

  def dfs(tree: BT[Double]): List[Double] =
    def search(nodesToVisit: BT[Double], result: List[Double]): List[Double] =
      nodesToVisit match
        case Empty => result
        case Node(value, leftSubtree, rightSubtree) =>  search(rightSubtree, search(leftSubtree, value :: result))
    search(tree, Nil)

  def splitPosition[A](list: List[A], position: Int): (List[A], List[A]) =
    @tailrec
    def splitPositionHelper(list: List[A], pos: Int, result: (List[A], List[A])): (List[A], List[A]) =
      (pos, list) match
        case (0, list) => (result._1, list)
        case (_, Nil) => (result._1, Nil)
        case (_, head :: tail) => splitPositionHelper(tail, pos - 1, (head :: result._1, Nil))
    splitPositionHelper(list, position, (Nil, Nil))

  def listLength[A](list: List[A]): Int =
    @tailrec
    def listLengthHelper(list: List[A], length: Int): Int =
      list match
        case head :: tail => listLengthHelper(tail, length + 1)
        case Nil => length
    listLengthHelper(list, 0)

  def assembleTreeFromList[A](listOfNodes: List[A]): BT[A] =
    listOfNodes match
      case Nil => Empty
      case head :: tail =>  val splitLists = splitPosition(tail, listLength(tail) / 2)
                            Node(head, assembleTreeFromList(splitLists._1), assembleTreeFromList(splitLists._2))

  def deleteDuplicatesBfs[A](tree: BT[A]): BT[A] =
    @tailrec
    def bfsQueue(queue: List[BT[A]], visitedValues: List[A]): List[A] =
      queue match
        case Nil => visitedValues
        case Empty :: t => bfsQueue(t, visitedValues)
        case Node(value, lTree, rTree) :: t =>  if visitedValues contains value then bfsQueue(t ::: List(lTree, rTree), visitedValues)
                                                else bfsQueue(t ::: List(lTree, rTree), value :: visitedValues)
    assembleTreeFromList(bfsQueue(tree :: Nil, Nil))

  def deleteDuplicatesDfs[A](tree: BT[A]): BT[A] =
    def search(nodesToVisit: BT[A], visitedValues: List[A]): List[A] =
      nodesToVisit match
        case Empty => visitedValues
        case Node(value, leftSubtree, rightSubtree) =>  if visitedValues contains value then search(rightSubtree, search(leftSubtree, visitedValues))
                                                        else search(rightSubtree, search(leftSubtree, value :: visitedValues))
    assembleTreeFromList(search(tree, Nil))

  def main(args: Array[String]) : Unit =
  {
    println("decToHex")
    println(decToHex(31) == List(1, 15))
    println(decToHex(25874) == List(6, 5, 1, 2))
    println(decToHex(2587499) == List(2, 7, 7, 11, 6, 11))
    println(decToHex(0) == List(0))
    println(decToHex(99999) == List(1, 8, 6, 9, 15))

    println("\ndecToSystem")
    println(decToSystem(0, 2) == List(0))
    println(decToSystem(14452, 2) == List(1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0))
    println(decToSystem(14452, 8)  == List(3, 4, 1, 6, 4))
    println(decToSystem(14452, 16) == List(3, 8, 7, 4))
    println(decToSystem(1, 16) == List(1))

    println("\ngenerateTree")
    println(generateTree(0))
    println(generateTree(1))
    println(generateTree(2))

    println("\ntreeProduct")
    println(treeProduct(Node(0.1, Node(0.2, Empty, Empty), Node(0.2, Empty, Empty))))
    println(treeProduct(Empty))
    println(treeProduct(generateTree(3)))
    println(treeProduct(generateTree(1)))
    println(treeProduct(generateTree(2)))

    println("\ndfs")
    println(dfs(Node(0.1, Node(0.2, Empty, Empty), Node(0.3, Empty, Empty))))
    println(dfs(Empty))
    println(dfs(Node(0.1,Empty, Empty)))
    println(dfs(Node(1.0, Node(2.0, Node(3.0, Empty, Empty), Node(4.0, Empty, Empty)), Node(5.0, Node(6.0, Empty, Empty), Node(7.0, Empty, Empty)))))

    println("\nbfs")
    println(bfs(Node(0.1, Node(0.2, Empty, Empty), Node(0.3, Empty, Empty))))
    println(bfs(Empty))
    println(bfs(Node(0.1,Empty, Empty)))
    println(bfs(Node(1.0, Node(2.0, Node(3.0, Empty, Empty), Node(4.0, Empty, Empty)), Node(5.0, Node(6.0, Empty, Empty), Node(7.0, Empty, Empty)))))

    println("\ndeleteDuplicatesBfs")
    println(deleteDuplicatesBfs(Node(0.1, Node(0.2, Empty, Empty), Node(0.1, Empty, Empty))))
    println(deleteDuplicatesBfs(Empty))
    println(deleteDuplicatesBfs(Node(1.0, Node(2.0, Node(4.0, Empty, Empty), Node(4.0, Empty, Empty)), Node(1.0, Node(4.0, Empty, Empty), Node(7.0, Empty, Empty)))))
    println(deleteDuplicatesBfs(Node(1.0, Node(2.0, Node(4.0, Empty, Empty), Node(5.0, Empty, Empty)), Node(1.0, Node(1.0, Empty, Empty), Node(1.0, Empty, Empty)))))

    println("\ndeleteDuplicatesDfs")
    println(deleteDuplicatesDfs(Node(0.1, Node(0.2, Empty, Empty), Node(0.1, Empty, Empty))))
    println(deleteDuplicatesDfs(Empty))
    println(deleteDuplicatesDfs(Node(1.0, Node(2.0, Node(4.0, Empty, Empty), Node(4.0, Empty, Empty)), Node(1.0, Node(4.0, Empty, Empty), Node(7.0, Empty, Empty)))))
    println(deleteDuplicatesDfs(Node(1.0, Node(2.0, Node(4.0, Empty, Empty), Node(5.0, Empty, Empty)), Node(1.0, Node(1.0, Empty, Empty), Node(1.0, Empty, Empty)))))
  }
}
