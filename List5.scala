import scala.annotation.tailrec
import scala.util.Random

object List5 {

  def divideWithRest(dividend: Int, divisor: Int):(Int, Int) =
    @tailrec
    def counter(result: Int, rest: Int):(Int, Int) =
      if rest >= divisor then counter(result+1, rest-divisor)
      else (result, rest)
    counter(0, dividend);

  def toNegative(base: Int, value: List[Int]):List[Int] =
    def reverser(tail: List[Int]): List[Int] =
      tail match
        case elem :: Nil => List(base - elem)
        case elem :: tl => ((base-1) - elem) :: reverser(tl)
        case Nil => Nil
    reverser(value)

  def changeNumberSystem(newBase: Int, decimal: Int):List[Int] =
    @tailrec
    def divider(rest: Int, result: List[Int], isNegative: Boolean):List[Int] =
      divideWithRest(rest, newBase) match
        case (0, rest) => if isNegative then 1::toNegative(newBase, rest::result)
                          else 0::rest::result
        case (product, rest) => divider(product,rest::result, isNegative)
    if decimal < 0 then
      divider(decimal * (-1), List(), true)
    else
      divider(decimal, List(), false);

  def decimalToHexa(decimal: Int):List[Int] =
    changeNumberSystem(16, decimal);

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def createTree(high: Int):BT[Double] =
    def iterator(i: Int): BT[Double] =
      i match
        case 0 => Empty
        case _ => Node(Random.nextDouble(), createTree(i-1), createTree(i-1))
    iterator(high)

  def treeProduct(tree: BT[Double]): Double =
    def iterator(subTree:BT[Double]): Double =
      subTree match
        case Empty => 1
        case Node(elem, left, right) => elem * iterator(left) * iterator(right)
    iterator(tree)

  def listToTree[A](values: List[A]):BT[A] =
    def creator(index: Int, length: Int): BT[A] =
      if  index >= length then Empty
      else if index == 0 then Node(values.apply(index),creator(1, length),creator(2 , length))
           else Node(values.apply(index),creator((2 * index) + 1, length),creator((2 * index) + 2, length))
    creator(0, values.length);

  def deleteDuplicatesDepth[A](tree: BT[A]): BT[A] =
    @tailrec
    def iterator(originalElems: List[A], stack:List[BT[A]]): List[A] =
      stack match
        case List() => originalElems.reverse
        case Empty :: tail => iterator(originalElems, tail)
        case Node(value, left, right) :: tail => if originalElems.contains(value)
                                         then iterator(originalElems, List(left, right) ::: tail)
                                         else iterator(value::originalElems, List(left, right) ::: tail)
    listToTree(iterator(List(), List(tree)));

  def deleteDuplicatesBreadth[A](tree: BT[A]): BT[A] =
    @tailrec
    def iterator(originalElems: List[A], queue: List[BT[A]]): List[A] =
      queue match
        case List() => originalElems.reverse
        case Empty :: tail => iterator(originalElems, tail)
        case Node(value, left, right)::tail => if originalElems.contains(value)
                                               then iterator(originalElems, tail ::: List(left, right))
                                               else iterator(value :: originalElems, tail ::: List(left, right))
    listToTree(iterator(List(), List(tree)));

  def breadthBT[A](bt: BT[A]): List[A] =
    def assist[A](res: List[BT[A]]): List[A] =
      res match {
        case Nil => Nil
        case Empty :: tail => assist(tail)
        case Node(value: A, leftSubTree: BT[A], rightSubTree: BT[A]) :: tail => value :: assist(tail ::: List(leftSubTree, rightSubTree))
      }
    assist(List(bt));

  def main(args: Array[String]): Unit = {

    println(changeNumberSystem(16,31))
    println(changeNumberSystem(16,-31))

    val tree = Node(3.0,Node(3.0,Node(1.0,Node(8.0, Empty, Empty),Node(5.0,Empty,Empty)),Node(7.5,Empty,Empty)),Node(8.0, Empty, Empty))
    val randomTree = createTree(4)
    println(breadthBT(randomTree))
    println(treeProduct(randomTree))
    println(breadthBT(tree))
    val tree2 = deleteDuplicatesDepth(tree)
    val tree3 = deleteDuplicatesBreadth(tree)
    println(breadthBT(tree2))
    println(breadthBT(tree3))
  }
}
