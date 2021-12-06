import scala.annotation.tailrec
import scala.util.Random

object Lab5 {


  //zadanie1
  def toHex(number:Int):List[Int] =
    if number < 0 then throw new Exception("negative number")
    @tailrec
    def toHexInner(number:Int, result:List[Int]):List[Int] =
      if number > 0 then toHexInner(number/16, (number%16)::result)
      else result
    toHexInner(number,List())

  def toOtherSystem(number:Int, system:Int):List[Int] =
    if number < 0 then throw new Exception("negative number")
    @tailrec
    def toOtherInner(number:Int, system:Int, result:List[Int]):List[Int] =
      if system == 0 then List()
      if number > 0 then toOtherInner(number/system, system, (number%system)::result)
      else result
    toOtherInner(number,system,List())

  //zadanie3

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def createTree(numberOfLevels:Int):BT[Double]=
    if numberOfLevels>0 then Node(scala.util.Random.nextDouble(),createTree(numberOfLevels-1),createTree(numberOfLevels-1))
    else Empty

  // funkcja do printowania drzewa
  def breadthBT[A](tree:BT[A]):List[A] =
    def breadth[A](queue:List[BT[A]]):List[A] =
      queue match
        case Nil => Nil
        case Empty::t => breadth(t)
        case Node(v,t1,t2)::t => v::breadth(t:::List(t1,t2))
    breadth(List(tree))

  //zadanie4

  def multiplyNodes(tree:BT[Double]):Double =
    @tailrec
    def breadthMultiply(queue:List[BT[Double]],result:Double):Double =
      queue match
        case Nil => result
        case Empty::t => breadthMultiply(t,result)
        case Node(v,t1,t2)::t => breadthMultiply(t:::List(t1,t2),result*v)
    breadthMultiply(List(tree),1.0)

  def multiplyNodes2(tree:BT[Double]):Double =
    tree match
      case Node(v,t1,t2) => v*multiplyNodes2(t1)*multiplyNodes2(t2)
      case Empty => 1

  // zadanie5

  def contains(list:List[Double],number:Double):Boolean =
    (list,number) match
      case(h::t,n) => if h==n then true else contains(t,n)
      case(Nil,n)  => false

  def atIndex(list:List[Double],index:Int):Double =
    (list,index) match
      case(h::t,1) => h
      case(Nil,_)  => -1.0
      case(h::t,i) => atIndex(t,i-1)




  def toTree(list:List[Double]):BT[Double] =
    def toTreeInner(list:List[Double],index:Int):BT[Double] =
      if atIndex(list,index) >= 0 then
        Node(atIndex(list,index),toTreeInner(list, index*2),toTreeInner(list, (index*2)+1))
      else Empty
    toTreeInner(list,1)


  def deleteDuplicatesBreadth(tree:BT[Double]):BT[Double] =
    def delete(queue:List[BT[Double]],newTree:List[Double]):List[Double] =
      queue match
        case Nil => newTree
        case Empty::t => delete(t,newTree)
        case Node(v,t1,t2)::t => if contains(newTree,v) then delete(t:::List(t1,t2),newTree) else delete(t:::List(t1,t2),v::newTree)
    toTree(delete(List(tree),List()))

  def deleteDuplicatesDepth(tree:BT[Double]):BT[Double] =
    def deleteInner(queue:List[BT[Double]],newTree:List[Double]):List[Double] =
      queue match
        case Nil => newTree
        case Empty::t => deleteInner(t,newTree)
        case Node(v,t1,t2)::t => if contains(newTree,v) then deleteInner(t1::t2::t,newTree) else deleteInner(t1::t2::t,v::newTree)
    toTree(deleteInner(List(tree),List()))

  def main(args: Array[String]) : Unit =
  {
    //println(toHex(31))
    //println(toOtherSystem(31,16))
    //val tree = createTree(2)
    //println(breadthBT(tree))
    //println(multiplyNodes(tree))
    //println(multiplyNodes2(tree))

    val tree = Node(1.0,Node(2.0,Node(1.0,Empty,Empty),Node(2.0,Empty,Empty)),Node(1.0,Node(1.0,Empty,Empty),Node(3.0,Empty,Empty)))
    println(tree)
    println(deleteDuplicatesBreadth(tree))
    val tree1 = Node(1.0,Node(2.0,Node(1.0,Empty,Empty),Node(2.0,Empty,Empty)),Node(1.0,Node(1.0,Empty,Empty),Node(3.0,Empty,Empty)))
    println(tree1)
    println(deleteDuplicatesDepth(tree1))
  }
}