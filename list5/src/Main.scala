import scala.annotation.tailrec
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

object Main {
  sealed trait BinTree[+A]
  case object Empty extends BinTree[Nothing]
  case class Node[+A](elem: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

  //1
  def decimalToHex(number: Int): List[Int] = {
    @tailrec
    def iterator(acc: Int, list: List[Int]) : List[Int]={
    if acc > 0 then
      iterator(acc/16, (acc % 16) :: list)
    else list
    }
    if number < 0 then List()
    else
      iterator(number, List())
  }// l16(n)

  //2
  def decimalToOther(number: Int, base: Int): List[Int] = {
    @tailrec
    def iterator(acc: Int, list: List[Int]) : List[Int]={
      if acc > 0 then
        iterator(acc/base, (acc % base) :: list)
      else list
    }
    if number < 0 then List()
    else
      iterator(number, List())
  }

  //3
  val rand = scala.util.Random()

  def generateTreeOfDepth(depth: Int): BinTree[Double] = {
    if depth <= 0 then Empty
    else
      Node(rand.nextFloat(), generateTreeOfDepth(depth - 1), generateTreeOfDepth(depth - 1))
  }

  //3
  def prodOfTree(tree: BinTree[Double]) : Double = {
    def prodHelper(subtree: BinTree[Double], acc: Double): Double = {
      subtree match {
        case Empty => acc
        case Node(value, left, right) => prodHelper(left, prodHelper(right, value * acc ))
      }
    }
    prodHelper(tree, 1.0)
  }//2^(n+1) - 1, gdzie n to liczba poziomów drzewa

  //4
  def getTreeListDFS(tree: BinTree[Double]): List[(Double, Int)] = {
    def visit(unique: List[Double], stack: List[(BinTree[Double], Int)], nodes: List[(Double, Int)] ): List[(Double, Int)] = {
      if stack.isEmpty then nodes
      else
      stack.head match {
        case (Empty, _) => visit(unique, stack.tail, nodes)
        case (Node(value, left, right), id) =>
          if unique.contains(value) then visit(unique, ((left, id * 2) :: (right, id * 2 + 1) :: stack.tail), (-1, id) :: nodes)
          else visit(value :: unique,  ((left, id * 2) :: (right, id * 2 + 1) :: stack.tail), (value, id) :: nodes)
      }
    }
    visit(List(), List((tree, 1)), List())
  }

  def getTreeListBFS(tree: BinTree[Double]): List[(Double, Int)] = {
    def visit(unique: List[Double], queue: List[(BinTree[Double], Int)], nodes: List[(Double, Int)] ): List[(Double, Int)] = {
      if queue.isEmpty then nodes
      else
        queue.head match {
          case (Empty, _) => visit(unique, queue.tail, nodes)
          case (Node(value, left, right), id) =>
            if unique.contains(value) then visit(unique, queue.tail ::: List((left, id * 2),(right, id * 2 + 1)), (-1, id) :: nodes)
            else visit(value :: unique, queue.tail ::: List((left, id * 2),(right, id * 2 + 1)), (value, id) :: nodes)
        }
    }
    visit(List(), List((tree, 1)), List())
  }

  def cleanUp(listOfNodes: List[(Double, Int)], id: Int ): List[(Double, Int)] = {
    if id <= listOfNodes.length then
      val List(x) = listOfNodes.filter(_._2 == id )
      if x._1 == -1 then
          val rep = getReplacement(id, listOfNodes)
          if rep._2 == -1 then cleanUp(listOfNodes, id + 1)
          else cleanUp(swapElements(listOfNodes, x, rep), id + 1)
      else cleanUp(listOfNodes, id + 1)
    else listOfNodes
  }

  def swapElements(list: List[(Double, Int)], first: (Double, Int), sec: (Double, Int)): List[(Double, Int)] = {
    def iterate(listToCheck: List[(Double, Int)]): List[(Double, Int)] = {
      if listToCheck != Nil then
        if listToCheck.head._2 == first._2 then (sec._1, first._2) :: iterate(listToCheck.tail)
        else if listToCheck.head._2 == sec._2 then (first._1, sec._2) :: iterate(listToCheck.tail)
        else listToCheck.head :: iterate(listToCheck.tail)
      else Nil
    }
    iterate(list)
  }

  def getReplacement(id: Int, listOfNodes: List[(Double, Int)]): (Double, Int) = {
    def replacingQueue(queue: List[Int]): (Double, Int) = {
      if queue != Nil then
        val found = listOfNodes.filter(_._2 == queue.head )
        if !found.isEmpty then
          found.head._1 match {
            case -1 => replacingQueue(queue.tail::: List(found.head._2 * 2, found.head._2 * 2 + 1))
            case _ => found.head
          }
        else replacingQueue(queue.tail)
      else (-1.0, 0)
    }
    replacingQueue(List(id * 2, id * 2 + 1))
  }

  def createTreeWithoutDuplicates(listOfNodes: List[(Double, Int)]): BinTree[Double] ={
    def createTree(id: Int): BinTree[Double] = {
      if id <= listOfNodes.length then
        val List(x) = listOfNodes.filter(_._2 == id )
        if x._1 == -1 then Empty
        else Node(x._1, createTree(id*2), createTree(id*2+1))
      else Empty
    }
    createTree(1)
  }

  def removeDuplicatesDFS(tree: BinTree[Double]): BinTree[Double] = {
    val treeList = getTreeListDFS(tree)
    val cleaned = cleanUp(treeList, 1)
    createTreeWithoutDuplicates(cleaned)
  }

  def removeDuplicatesBFS(tree: BinTree[Double]): BinTree[Double] = {
    val treeList = getTreeListBFS(tree)
    val cleaned = cleanUp(treeList, 1)
    createTreeWithoutDuplicates(cleaned)
  }

  def main(args: Array[String]): Unit ={
    //    println(decimalToHex(1128));
    //    println(decimalToHex(256));
    //    println(decimalToHex(921));
    //    println(decimalToHex(188));
    //    println(decimalToHex(590));
    //    println(decimalToOther(10, 2));
    //    println(decimalToOther(1458, 2));
    //    println(decimalToOther(10, 4));
    //    println(decimalToOther(1000, 8));

    val tree = generateTreeOfDepth(3);
    var dupTree = Node(5.0,Node(5.0, Node(7.0, Empty, Empty), Node(2.0, Empty, Empty)), Node(2.0,Node(1.0, Empty, Empty) ,Node(3.0, Empty, Empty) ));
    var hardTree = Node(5.0, Node(5.0, Node(5.0, Node(7.0, Empty, Empty), Node(4.0, Empty, Empty)), Node(2.0, Node(6.0, Empty, Empty), Node(8.0, Empty, Empty))), Node(2.0, Node(1.0, Node(9.0, Empty, Empty), Node(10.0, Empty, Empty)), Node(3.0, Node(11.0, Empty, Empty), Node(12.0, Empty, Empty))))

    println(tree);

    //    println(breadthBT(tree));
        println(prodOfTree(tree));
    //    println(breadthBT(generateTreeOfDepth(2)));
    //    println(breadthBT(generateTreeOfDepth(1)));
    //    println(breadthBT(generateTreeOfDepth(0)));


    val dupListTree = getTreeListBFS(dupTree)
    println(dupListTree);
    println(removeDuplicatesBFS(dupTree))

    //      println(cleanUpDFS(hardListTree, 1))
    //        println(removeDuplicatesDFS(hardTree))


  }

}
