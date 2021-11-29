import main.Node

import scala.annotation.tailrec
import scala.util.Random;

object main {

  //zadanie 1

  def toHex(number: Int): List[Int] =
    @tailrec
    def changeTail(rest: Int, list: List[Int]): List[Int] =
      if rest / 16 < 1 then
        rest :: list
      else
        changeTail((rest / 16), (rest % 16) :: list)

    changeTail(number, Nil)


  def changeNumeralSystem(number: Int, system: Int): List[Int] =
    @tailrec
    def changeTail(rest: Int, list: List[Int]): List[Int] =
      if rest / system < 1 then
        rest :: list
      else
        changeTail((rest / system), (rest % system) :: list)

    changeTail(number, Nil)


  //definicja drzewa
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //zadanie 3
  def createTree(level: Int): BT[Double] = {
    if level == 1 then
      Node(math.random(), Empty, Empty)
    else
      Node(math.random(), createTree(level - 1), createTree(level - 1))
  }

  def createTreeInt(level: Int): BT[Int] = {
    if level == 1 then
      Node(Random.nextInt(6), Empty, Empty)
    else
      Node(Random.nextInt(6), createTreeInt(level - 1), createTreeInt(level - 1))
  }

  //zadanie 4
  def productOfNodes(tree: BT[Double]): Double = {
    @tailrec
    def productTail[A](queue: List[BT[Double]], result: Double): Double = {
      queue match
        case Nil => result
        case Empty :: tq => productTail(tq, result)
        case Node(elem, left, right) :: tq => productTail(left :: right :: tq, elem * result)
    }

    productTail(List(tree), 1)
  }

  //przejście drzewa wrzerz
  def breadthBT[A](bt: BT[A]): List[A] = {
    def BST[A](queue: List[BT[A]]): List[A] = {
      (queue) match
        case (Nil) => Nil
        case (Empty :: tq) => BST(tq)
        case (Node(v, t1, t2) :: tq) => v :: BST(tq ::: List(t1, t2))
    }

    BST(List(bt))
  }

  //przejście drzewa wgłąb
  def depthBT[A](bt: BT[A]): List[A] = {
    def BST[A](stack: List[BT[A]]): List[A] = {
      (stack) match
        case (Nil) => Nil
        case (Empty :: tq) => BST(tq)
        case (Node(v, t1, t2) :: tq) => v :: BST(t1 :: t2 :: tq)
    }

    BST(List(bt))
  }

  //zadanie 5

  def deleteDupicatesBreadthBT[A](bt: BT[A]): BT[A] = {
    @tailrec
    def createNodeList[A](queue: List[BT[A]], listOut: List[A]): List[A] = {
      (queue) match
        case (Nil) => listOut
        case (Empty :: tq) => createNodeList(tq, listOut)
        case (Node(v, Empty, Empty) :: tq) => createNodeList((tq), checkDuplicates(v, listOut))
        case (Node(v, Empty, t2) :: tq) => createNodeList((tq ::: List(t2)), checkDuplicates(v, listOut))
        case (Node(v, t1, Empty) :: tq) => createNodeList(tq ::: List(t1), checkDuplicates(v, listOut))
        case (Node(v, t1, t2) :: tq) => createNodeList(tq ::: List(t1, t2), checkDuplicates(v, listOut))
    }

    val tree=createNodeList(List(bt), List())
    buildTree(tree, 1, sizeOfTree(tree))
  }


  def deleteDuplicatesDepthBT[A](bt: BT[A]): BT[A] = {
    @tailrec
    def createNodeList[A](queue: List[BT[A]], listOut: List[A]): List[A] = {
      (queue) match
        case (Nil) => listOut
        case (Empty :: tq) => createNodeList(tq, listOut)
        case (Node(v, Empty, Empty) :: tq) => createNodeList((tq), checkDuplicates(v, listOut))
        case (Node(v, Empty, t2) :: tq) => createNodeList((t2 :: tq), checkDuplicates(v, listOut))
        case (Node(v, t1, Empty) :: tq) => createNodeList((t1 :: tq), checkDuplicates(v, listOut))
        case (Node(v, t1, t2) :: tq) => createNodeList(t1 :: t2 :: tq, checkDuplicates(v, listOut))
    }
    val tree = createNodeList(List(bt), List())
    buildTree(tree, 1, sizeOfTree(tree))
  }

  def checkDuplicates[A](value: A, listOut: List[A]): List[A] = {
    listOut match
      case Nil => List(value)
      case (h :: t) =>
        if h == value then listOut
        else h :: checkDuplicates(value, t)
  }

  @tailrec
 def findNode[A](valueList: List[A], index: Int):A= {
   (valueList, index) match
     case (List(), _) => throw new Exception("Error, wrong index or empty list")
     case (head::_, 0) => head
     case (_::tail, _) => findNode(tail, index-1)
 }

  def sizeOfTree[A](nodesList: List[A]):Int={
    @tailrec
    def sizeInner(nodesList: List[A], size: Int):Int=
      nodesList match
        case List() => size
        case h::t  => sizeInner(t, size+1)
    sizeInner(nodesList, 0)
  }

  def buildTree[A](valueList: List[A], index: Int, treeSize: Int):BT[A]={
    if treeSize<index then Empty
    else Node(findNode(valueList, index-1), buildTree(valueList, 2*index, treeSize), buildTree(valueList, 2*index+1, treeSize))
  }


  def main(args: Array[String]): Unit = {

    println("\nZadanie 1:")
    println(toHex(10))
    println(toHex(31))
    println(toHex(101))
    println(changeNumeralSystem(10,2))
    println(changeNumeralSystem(31,16))

    println("\nZadanie 3 i 4:")
    val tree2=createTree(2)
    println(breadthBT(tree2))
    println(productOfNodes(tree2))

    val tree3=createTree(3)
    println(breadthBT(tree3))
    println(productOfNodes(tree3))


    println("\nZadanie 5:")

    val treeInt3=createTreeInt(4)
    println(treeInt3)
    println(deleteDupicatesBreadthBT(treeInt3))
    println(treeInt3)
    println(deleteDuplicatesDepthBT(treeInt3))

  }
}
