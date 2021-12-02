package Test

import scala.annotation.tailrec

object Laboratorium5 {

  def fromDecToHex(number: Int): List[Int] = {

    @tailrec
    def fromDecToHexHelp(decNumber: Int, hexNumber: List[Int]): List[Int] = {
      if decNumber == 0 then hexNumber
      else fromDecToHexHelp(decNumber / 16, decNumber % 16 :: hexNumber)
    }

    fromDecToHexHelp(number, List())
  }

  def fromDexToOther(numberAndSystem: List[Int]): List[Int] = {
    def fromDexToOtherHelp(decNumber: Int, system: Int, number: List[Int]): List[Int] = {
      if decNumber == 0 then number
      else fromDexToOtherHelp(decNumber / system, system, decNumber % system :: number)
    }

    fromDexToOtherHelp(numberAndSystem(0), numberAndSystem(1), List())
  }

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def makeRandomTree(depth: Int): BT[Float] = {
    def makeRandomTreeHelper(depth: Int, curDepth: Int): BT[Float] = {
      val rnd = new scala.util.Random
      if depth == curDepth then Empty
      else Node(rnd.nextFloat(), makeRandomTreeHelper(depth, curDepth + 1), makeRandomTreeHelper(depth, curDepth + 1))
    }

    makeRandomTreeHelper(depth, 0)
  }

  def productOfAllElements(node: BT[Float]): Float = {
    def productOfAllElementsHelper(node: BT[Float]): Float = {
      node match {
        case Empty => 1
        case Node(key, left, right) => key * productOfAllElementsHelper(left) * productOfAllElementsHelper(right)
      }
    }

    if (node != Empty) {
      productOfAllElementsHelper(node);
    } else 0
  }

  def deletingDFS(node: BT[Int]): BT[Int] = {
    @tailrec
    def deletingDFSHelper(queue: List[BT[Int]], list: List[Int]): List[Int] = {
      queue match {
        case List() => list
        case Empty :: tailQueue => deletingDFSHelper(tailQueue, list)
        case Node(value, Empty, Empty) :: tailQueue => deletingDFSHelper(tailQueue, duplication(value, list))
        case Node(value, Empty, right) :: tailQueue => deletingDFSHelper(right :: tailQueue, duplication(value, list))
        case Node(value, left, Empty) :: tailQueue => deletingDFSHelper(left :: tailQueue, duplication(value, list))
        case Node(value, left, right) :: tailQueue => deletingDFSHelper(right :: left :: tailQueue, duplication(value, list))
      }
    }

    val tree = deletingDFSHelper(List(node), List())
    makeTree(tree, 1, sizeTree(tree))
  }

  def deletingBFS(node: BT[Int]): BT[Int] = {
    @tailrec
    def deletingBFSHelper(queue: List[BT[Int]], list: List[Int]): List[Int] = {
      queue match {
        case List() => list
        case Empty :: tailQueue => deletingBFSHelper(tailQueue, list)
        case Node(value, Empty, Empty) :: tailQueue => deletingBFSHelper(tailQueue, duplication(value, list))
        case Node(value, Empty, right) :: tailQueue => deletingBFSHelper(tailQueue ::: List(right), duplication(value, list))
        case Node(value, left, Empty) :: tailQueue => deletingBFSHelper(tailQueue ::: List(left), duplication(value, list))
        case Node(value, left, right) :: tailQueue => deletingBFSHelper(tailQueue ::: List(left, right), duplication(value, list))
      }
    }

    val tree = deletingBFSHelper(List(node), List())
    makeTree(tree, 1, sizeTree(tree))
  }

  def makeTree(valueList: List[Int], index: Int, treeSize: Int): BT[Int] = {
    if treeSize < index then Empty
    else Node(findNode(valueList, index - 1), makeTree(valueList, 2 * index, treeSize), makeTree(valueList, 2 * index + 1, treeSize))
  }

  @tailrec
  def findNode(value: List[Int], i: Int): Int = {
    (value, i) match
      case (List(), _) => -1
      case (head :: _, 0) => head
      case (_ :: tail, _) => findNode(tail, i - 1)
  }

  def duplication(value: Int, list: List[Int]): List[Int] = {
    list match {
      case Nil => List(value)
      case h :: t => if h == value then list else h :: duplication(value, t)
    }
  }

  def sizeTree(nodes: List[Int]): Int = {
    @tailrec
    def sizeTreeHelper(nodes: List[Int], size: Int): Int = {
      nodes match {
        case List() => size
        case h :: t => sizeTreeHelper(t, size + 1)
      }
    }

    sizeTreeHelper(nodes, 0)
  }

  def main(args: Array[String]): Unit = {

    println(fromDecToHex(31) == List(1, 15))
    println(fromDecToHex(45) == List(2, 13))
    println(fromDecToHex(16) == List(1, 0))
    println(fromDecToHex(4444) == List(1, 1, 5, 12))

    println(fromDexToOther(List(31, 16)) == List(1, 15))
    println(fromDexToOther(List(31, 2)) == List(1, 1, 1, 1, 1))
    println(fromDexToOther(List(31, 8)) == List(3, 7))


    val tree = makeRandomTree(2)

    println(tree)

    println(productOfAllElements(tree))


    val tree1 = Node(1, Node(2, Node(3, Node(4, Node(10, Node(3, Empty, Empty), Node(15, Empty, Empty)),
      Node(11, Empty, Empty)), Node(7, Empty, Empty)), Node(9, Node(5, Empty, Empty), Node(6, Empty, Empty))),
      Node(3, Node(3, Empty, Empty), Empty))
    val tree2 = Node(1, Node(2, Empty, Empty), Node(1, Node(3, Empty, Empty), Node(4, Empty, Empty)))
    val tree3 = Empty

    println(deletingDFS(tree1))
    println(deletingDFS(tree2))
    println(deletingDFS(tree3))

    println(deletingBFS(tree1))
    println(deletingBFS(tree2))
    println(deletingBFS(tree3))
  }


}