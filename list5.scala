import org.w3c.dom.html.HTMLDListElement

import scala.annotation.tailrec
import scala.util.Random
object list5 {

  //zadanie 1
  def decToHex(dec:Int): List[Int]={
    def decToHexHelp(dec:Int): List[Int]={
      if dec == 0 then Nil
      else dec % 16 :: decToHexHelp(dec / 16)
    }
    decToHexHelp(dec).reverse
  }

  def decToHexBetter(dec: Int): List[Int]={
    @tailrec
    def decToHexBetterHel(dec: Int, list: List[Int]): List[Int]={
      if dec<16 then dec::list
      else if dec<0 then decToHexBetterHel((-dec)/16, (-dec%16)::list:::List(-1))
      else decToHexBetterHel(dec/16, dec%16::list)
    }
    var result = decToHexBetterHel(dec, List())
    if result.reverse.head == -1 then -1::result.reverse.tail.reverse
    else result
  }
  //zadanie 1 - mod *
  def decToAny(dec:Int, sys:Int):List[Int]={
    def decToAnyHelp(dec:Int):List[Int]={
      if dec==0 then Nil
      else dec % sys ::decToAnyHelp(dec / sys)
    }
    decToAnyHelp(dec).reverse
  }

  def decToAnyBetter(dec:Int, sys:Int):List[Int]={
    @tailrec
    def decToAnyHelp(dec:Int, list: List[Int]):List[Int]={
      if sys<0 then throw new Exception("Negative base")
      else if dec<0 then decToAnyHelp(-dec/sys, (-dec%(sys))::list:::List(-1))
      else if dec<sys then dec::list
      else decToAnyHelp(dec/sys, dec%sys::list)
    }
    var result = decToAnyHelp(dec, List())
    if result.reverse.head == -1 then -1::result.reverse.tail.reverse
    else result
  }
  //zadanie 3
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]
  def createTree(height: Double, start:Double, end:Double): BT[Double] ={
    val random = new Random().nextDouble();
    val value = start + (random * (end - start))
    if height>1 then Node(value, createTree(height-1, start, value), createTree(height-1, value, end))
    else if height==0 then Empty
    else Node(value, Empty, Empty)
  }
  def BFS[A](bt: BT[A]): List[A] ={
    @tailrec
    def bread[A](queue: List[BT[A]], result: List[A]):List[A]= {
      if queue == Nil then result
      else
        queue.head match {
          case Empty => bread(queue.tail, result)
          case Node(elem, left, right) => bread(queue.tail ::: List(left, right), elem :: result)
        }
    }
    bread(List(bt), List()).reverse
  }
  //zadanie 4
  def multiplyNodes(bt: BT[Double]): Double ={
    @tailrec
    def bread(queue: List[BT[Double]], result: Double):Double={
      if queue==Nil then result
      else
        queue.head match {
          case Empty => bread(queue.tail, result)
          case Node(elem, left, right) => bread(queue.tail:::List(left, right), elem*result)
        }
    }
    bread(List(bt), 1.0)
  }
  //zadanie 5

  def returnValue(node: BT[Int]): Int = {
    node match {
      case Node(elem, _, _) => elem
    }
  }

  def treeMinimum (node: BT[Int]): BT[Int]={
    node match{
      case Node(elem, left, Empty) => node
      case Node(elem, left, right) => treeMinimum(right)
    }
  }
  def minValue(node: BT[Int], value:Int): Int={
    node match{
      case Node(elem, left, _) => minValue(left, returnValue(left))
      case Node(elem, Empty, _) => value
    }
  }
  def treeSucessor (node:BT[Int]): BT[Int]={
    node match{
      case Node(elem, left, right) => if (right != Empty) {
        treeMinimum(right)
      }
      else{
        treeSucessor(Node(3, Empty, Empty))
      }
    }
  }
  def createTreeInt(height: Int, start:Int, end:Int): BT[Int] ={
    var random = new scala.util.Random
    val value = start+ random.nextInt( (end - start) + 1 )
    if height>1 then Node(value, createTreeInt(height-1, start, value), createTreeInt(height-1, value, end))
    else if height==0 then Empty
    else Node(value, Empty, Empty)
  }

  def check(queue: List[Int], elem: Int): Boolean={
    queue match{
      case (h::t)=>if(h==elem)then true else check(t, elem)
      case Nil => false
    }
  }
  def Delete(node: BT[Int], key: Int): BT[Int]={
    if node ==null then node
    else node match{
      case Node(elem, Empty, right) => right
      case Node(elem, left, Empty) => left
      case Node(elem, left, right) => {
        var temp = minValue(right, returnValue(right))
        Delete(right, temp)
      }
    }
  }

  def DeletingByDfs(bt: BT[Int]): BT[Int]={
    def Helper(tree: BT[Int], listElements: List[Int]):BT[Int]={
      tree match{
        case Empty => Empty
        case Node(elem, Empty, right)=>if(check(listElements, elem)==true) then Helper(Delete(tree, elem), listElements) else
          Node(elem, Empty, Helper(right, elem::listElements))
        case Node(elem, left, Empty)=>if(check(listElements, elem)==true) then Helper(Delete(tree, elem), listElements) else
          Node(elem, Helper(left, elem::listElements), Empty)
        case Node(elem, left, right) => if(check(listElements, elem)==true) then Helper(Delete(tree, elem), listElements) else
          Node(elem, Helper(left, elem::listElements), Helper(right, elem::listElements))
      }
    }
    Helper(bt, List())
  }

  def insert(tree: BT[Int], elem:Int): BT[Int]={
    tree match{
      case Empty => Node(elem, Empty, Empty)
      case Node(root, right, left) => if root<elem then Node(root, right, insert(left, elem)) else
        Node(root, insert(right, elem), left)
    }
  }
  def buildTree(queue: List[Int], tree: BT[Int]): BT[Int]={
    queue match{
      case Nil => tree
      case h::t => buildTree(t, insert(tree, h))
    }
  }

  def DFS[A](bt: BT[A]): (List[A])={
    @tailrec
    def DFSHelper(stack: List[BT[A]], result: List[A]): (List[A]) ={
      if stack==Nil then result
      else
        stack.head match {
          case Empty => DFSHelper(stack.tail, result)
          case Node(elem, left, right) => DFSHelper(List(left, right):::stack.tail, elem::result)
        }
    }
    DFSHelper(List(bt), List()).reverse
  }
  /*def DeletingByDfs(bt :BT[Int]): (BT[Int],List[Int])= {
    def DfsInner(stack: List[Int], listElements: List[Int], visited: List[Int]): (List[Int])= {
      stack match{
        case Nil => visited
        case h::t => if(check(listElements, h)==true) then DfsInner(t, listElements, visited) else
          DfsInner(t, h::listElements, visited:::List(h))
      }
    }
    (buildTree(DfsInner(DFS(bt), List(), List()), Empty), DfsInner(DFS(bt), List(), List()))

  }*/

  def DeletingByBfs(bt: BT[Int]): (BT[Int])= {
    def BfsInner(queue: List[Int], listElements: List[Int], visited: List[Int]): (List[Int])= {
      queue match {
        case Nil => visited
        case h::t => if(check(listElements, h)==true) then BfsInner(t, listElements, visited) else BfsInner(t, h::listElements, visited:::List(h))
      }
    }
    buildTree(BfsInner(BFS(bt), List(), List()), Empty)
  }

  def main(args: Array[String]): Unit = {
    println(decToHexBetter(31))
    println(decToHexBetter(100))
    println(decToAnyBetter(31, 16))
    println(decToAnyBetter(-2936, 16))
    println(decToAnyBetter(100,8))
    println(BFS(createTree(3, 0, 1)))
    val t = createTree(3, 0, 1)
    println(BFS(t))
    println(multiplyNodes(t))
    println("")

    println("Deleting by DFS and BFS:")
    var t2 = createTreeInt(3, 0, 15)
    var tt2 = buildTree(BFS(t2), Empty)
    println(BFS(tt2))
    println(DFS(t2))
    println(t2)
    println("Deleted by BFS:")
    println((DeletingByBfs(t2)))
    //println(BFS(DeletingByBfs(t2)))
    println("Deleted by DFS:")
    println((DeletingByDfs(tt2)))
    //println(BFS(DeletingByDfs(tt2)))
    /*println("Deleting by BFS:")
    var t3 = createTreeInt(3, 0, 20)
    println(t3)
    println(BFS(t3))
    println(DeletingByBfs(t3))
    println(BFS(DeletingByBfs(t3)))*/
  }

}
