import javax.xml.transform.Result
import scala.annotation.tailrec
import scala.util.Random

object l3 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def main(args: Array[String]): Unit = {

    //zadanie 1
    println("Zadanie 1")
    println(hexidecimalChange(31))
    println(hexidecimalChange(-31))
    println("Zadanie 2")
    println(fromDecimalToChosen(31,16))
    println(fromDecimalToChosen(-31,16))
    println("Zadanie 3")
    val tree = createBinaryTree(3)
    println(tree)
    println("Zadanie 4")
    println(multiplyTreeNodes(tree))
    println("Zadanie 5")
    val treee = Node(1.0, Node(2.0, Node(1.0,Empty,Empty), Node(5.0,Empty, Empty)), Node(3.0,Node(4.0,Empty,Empty), Node(5.0,Empty,Empty)))
    println("original")
    println(treee)
    println("DFS")
    println(deleteDuplicatesDFS(treee))
    println("original")
    println(treee)


  }

  //zadanie 1
  def hexidecimalChange(number:Int): List[Int] = {
    @tailrec
    def hexidecimalHelper(numberHelper:Int, list:List[Int]): List[Int] = {
      if(numberHelper > 0) then
        val checkNumber = numberHelper % 16
        hexidecimalHelper(numberHelper/16, checkNumber::list)
      else list
    }
    if (number <= 0) then List()
    else hexidecimalHelper(number, List())
  }

  //zadanie 2
  def fromDecimalToChosen(number:Int, system:Int): List[Int] = {
    @tailrec
    def fromDecimalHelper(numberHelper:Int, systemHelper:Int, result:List[Int]): List[Int] = {
      if(numberHelper > 0) then
        val checkNumber = numberHelper % systemHelper
        fromDecimalHelper(numberHelper/system , systemHelper,checkNumber::result)
      else result
    }
    if (number <= 0) then List()
    else fromDecimalHelper(number,system,List())
  }

  //zadanie 3
  def createBinaryTree(depth:Int):BT[Double] = {
    depth match
      case 0 => Empty
      case _ => Node(Random.nextDouble(), createBinaryTree(depth - 1), createBinaryTree(depth - 1))
  }

  //zadanie 4
  def multiplyTreeNodes(tree:BT[Double]): Double = {
    def multiplyTreeNodesHelper(toVisit: BT[Double], result:Double): Double = {
      toVisit match
        case Empty => result
        case Node(value, left, right) => multiplyTreeNodesHelper(left, multiplyTreeNodesHelper(right, value * result))
    }
    multiplyTreeNodesHelper(tree,1)
  }

  //zadanie 5 - DFS
  def depthSearch(tree:BT[Float]): List[Float] = {
    def depthSearchHelper(treeList:List[BT[Float]]): List[Float] = {
      treeList match {
        case Nil => Nil
        case Empty :: tail => depthSearchHelper(tail)
        case Node(value, left, right) :: tail => value :: depthSearchHelper(List(left, right) ::: tail)
      }
    }
    depthSearchHelper(List(tree))
  }

  def deleteDuplicatesDFS(tree:BT[Double]) = {
    @tailrec
    def deleteDuplicatesDFSHelper(treeList:List[BT[Double]], resultTreeList:List[Double]): List[Double] = {
      treeList match
        case Nil => resultTreeList
        case (Empty :: tail) => deleteDuplicatesDFSHelper(tail, resultTreeList)
        case (Node(value, Empty, Empty) :: tail) => deleteDuplicatesDFSHelper(tail, changeDuplicates(value, resultTreeList))
        case (Node(value, leftTail, rightTail)::tail) =>deleteDuplicatesDFSHelper(leftTail::rightTail::tail, changeDuplicates(value, resultTreeList))
    }
    generateTreeFromList(deleteDuplicatesDFSHelper(List(tree), List()))
  }

  def changeDuplicates(value:Double, list:List[Double]): List[Double] = {
    list match {
      case Nil => List(value)
      case head::tail =>
        if (head == value) then list:::List(0.0)
        else head::changeDuplicates(value, tail)
    }
  }



  //przeszukiwanie wszerz
  def breadthBT[A](tree:BT[A]) = {
    def breadthHelper[A](help: List[BT[A]]): List[A] = {
      help match
        case Nil => Nil
        case Empty :: tail => breadthHelper(tail)
        case Node(value, left, right) :: tail => value :: breadthHelper(tail ::: List(left, right))
    }

    breadthHelper(List(tree))
  }

  def deleteDuplicatesBFS(tree:BT[Double]) = {
    @tailrec
    def deleteDuplicatesBFSHelper(treeList:List[BT[Double]], resultTreeList:List[(Double)]): List[(Double)] = {
      treeList match
        case Nil => resultTreeList
        case (Empty :: tail) => deleteDuplicatesBFSHelper(tail, resultTreeList)
        case (Node(value, Empty, Empty) :: tail) => deleteDuplicatesBFSHelper(tail, changeDuplicates(value, resultTreeList))
        case (Node(value, leftTail, rightTail)::tail) =>deleteDuplicatesBFSHelper(tail:::List(leftTail,rightTail), changeDuplicates(value, resultTreeList))

    }
    generateTreeFromList(deleteDuplicatesBFSHelper(List(tree), List()))
  }


  //metody pomocnicze
  def generateTreeFromList(list:List[Double]): BT[Double] = {
    list match {
      case Nil => Empty
      case h::t =>
        val splitedLists = t.splitAt(t.length/2)
        Node(h, generateTreeFromList(splitedLists._1), generateTreeFromList(splitedLists._2))
    }
  }

}


