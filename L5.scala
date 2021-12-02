import scala.annotation.tailrec
import scala.util.Random
object L5 {

  def toHex(number:Int):List[Int] = {
    @tailrec
    def helper(number:Int, res:List[Int]):List[Int] = {
      if number == 0 then res
      else helper(number/16,number%16::res)
    }
    if number <0 then (-1)::helper(-number,Nil)
    else helper(number,Nil)
  }

  def fromDecToAny(number: Int,base:Int):List[Int] = {
    @tailrec
    def helper(number:Int,base:Int,res:List[Int]):List[Int] = {
      if number==0 then res
      else helper(number/base,base,number%base::res)
    }
    if number<0 && base<2 then Nil
    else if number <0 then (-1)::helper(-number,base, Nil)
    else helper(number,base,Nil)

  }

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


  def generateTree(depth: Int): BT[Float] = {
    def helper(depth: Int, currentDepth: Int): BT[Float] = {
      val rnd = Random()
      if depth == currentDepth then Empty
      else Node(rnd.nextFloat(), helper(depth, currentDepth+1), helper(depth, currentDepth+1))
    }
    helper(depth, 0)
  }

  def multiplyTreeValues(node: BT[Float]): Float = {
    if node == Empty then 0
    else
      def helper(node: BT[Float]): Float = {
        node match
          case Empty => 1
          case Node(key, leftNode, rightNode) => key * helper(leftNode) *helper(rightNode)
      }
      helper(node)
  }

  def depthBT[A](bt: BT[A]): List[A] = {
    def helper[A](stack: List[BT[A]]): List[A] = {
      (stack) match
        case (Nil) => Nil
        case (Empty :: stackTail) => helper(stackTail)
        case (Node(value, subTree1, subTree2) :: stackTail) => value :: helper(subTree1 :: subTree2 :: stackTail)
    }
    helper(List(bt))
  }

  def breadthBT[A](bt: BT[A]): List[A] = {
    def BST[A](queue: List[BT[A]]): List[A] = {
      (queue) match
        case (Nil) => Nil
        case (Empty :: queueTail) => BST(queueTail)
        case (Node(value, subTree1, subTree2) :: queueTail) => value :: BST(queueTail ::: List(subTree1, subTree2))
    }
    BST(List(bt))
  }

@tailrec
  def findValue[A](valueList: List[A], index: Int):A= {
    (valueList, index) match
      case (List(), _) => throw new Exception("Error")
      case (head::_, 0) => head
      case (_::tail, _) => findValue(tail, index-1)
  }

  def sizeOfTree[A](nodesList: List[A]):Int={
    @tailrec
    def sizeInner(nodesList: List[A], size: Int):Int=
      nodesList match
        case List() => size
        case head::tail  => sizeInner(tail, size+1)
    sizeInner(nodesList, 0)
  }

  def buildTree[A](valueList: List[A], index: Int, treeSize: Int):BT[A]={
    if treeSize<index then Empty
    else Node(findValue(valueList, index-1), buildTree(valueList, 2*index, treeSize), buildTree(valueList, 2*index+1, treeSize))
  }


  def deleteDuplicates[A](list:List[A]): List[A] ={
    @tailrec
    def helper[A](list:List[A],resList:List[A]):List[A]={
      list match {
        case Nil => resList.reverse
        case h::t => if resList.contains(h) then helper(t,resList) else helper(t,h::resList)
      }
    }
    helper(list,List())
  }



  def deleteDepth[A](tree:BT[A]):BT[A] = {
    val treeList = deleteDuplicates(depthBT(tree))
    buildTree(treeList,1,sizeOfTree(treeList))
  }


  def deleteBreadth[A](tree: BT[A]):BT[A] = {
    val treeList = deleteDuplicates(breadthBT(tree))
    buildTree(treeList,1,sizeOfTree(treeList))
  }
  def replaceString(string:String):String ={
    string+"(1)"
  }
  def replaceDuplicates(list: List[String]):List[String] = {
    @tailrec
    def helper(list:List[String],resList:List[String]):List[String]={
      list match {
        case Nil => resList.reverse
        case h::t => if resList.contains(h) then helper(t,replaceString(h)::resList) else helper(t,h::resList)
      }
    }
    helper(list,List())
  }

  def countDuplicates[A](list: List[A]):Int = {
    def helper[A](list: List[A],res:Int):Int = {
      list match {
        case Nil => res
        case h::t => if list.contains(h) then helper(t,res+1) else helper(t,res)
      }
    }
    helper(list,0)
  }

  def replaceduplicates2(list:List[String]):List[String] = {
    def helper(list: List[String],left:Int):List[String] = {
      if left>0 then helper(replaceDuplicates(list),left-1)
      else list
    }
    helper(list,countDuplicates(list))
  }

  def replaceBreadth(tree:BT[String]):BT[String] = {
    val treeList = replaceduplicates2(breadthBT(tree))
    buildTree(treeList,1,sizeOfTree(treeList))
  }

  def main(args: Array[String]): Unit = {

    println(toHex(-255))
    println(fromDecToAny(-255,1))
    println()
    val tree1 = generateTree(1)
    val tree2 = generateTree(3)
    val tree3 = generateTree(0)

    println(tree1)
    println(tree2)
    println(tree3)
    println()
    println(multiplyTreeValues(tree2))


    println()

    val tree4 = Node(1, Node(2, Node(4,Empty,Empty), Node(3,Empty,Empty)), Node(4, Node(5, Empty, Empty), Node(6, Empty, Empty)))
    println(tree4)
    println(deleteDepth(tree4))
    val tree5 = Node("szkola",Node("pracaDomowa",Node("pracaDomowa",Empty,Empty),Node("pracaDomowa",Empty,Empty)),Node("pracaDomowa",Empty,Empty))
    println()
    println(tree5)
    println(replaceBreadth(tree5))

    //println(replaceBreadth(tree5))




  }

}
