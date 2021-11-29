import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object Main {
    sealed trait BT[A]
  case class Empty[A]() extends BT[A]
  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


    def toHex(number: Int): List[Int]=
      @tailrec
      def toHexTailrec(number: Int, list: List[Int]): List[Int]=
        if number != 0 then
          toHexTailrec(number/16, List(number%16) ::: list)
        else list
      toHexTailrec(number, Nil)

  //zdecydowałem się na dwa argumenty zamiast listy dla przejrzystości użycia tej funkcji
    def changeBase(number: Int, base: Int): List[Int]=
      @tailrec
      def changeBaseTailrec(number: Int, list: List[Int]): List[Int]=
        if number != 0 then
          changeBaseTailrec(number/base, List(number%base) ::: list)
        else list
      changeBaseTailrec(number, Nil)

    def createNLevelTree(n: Int):Node[Double] =
      if n==1 then Node(Random.nextDouble(),Empty[Double](),Empty[Double]())
      else Node(Random.nextDouble(),createNLevelTree(n-1),createNLevelTree(n-1))

    def treeProduct(tree: BT[Double]): Double =
      tree match
        case Empty() => 1
        case Node(elem, left, right) => elem*treeProduct(left)*treeProduct(right)
    def contains[A](list: List[A], elem: A): Boolean =
      list match
        case Nil => false
        case h::t => if h == elem then true else contains(t, elem)

    def depthFirstSearch[A](node: BT[A], list: List[A], parrent: BT[A]): Unit=
      node match
        case Empty() =>
        case Node(elem, left, right) =>
          depthFirstSearch(left, list, node)
          if contains(list, elem) then
            if parrent.right == node
            then parrent.right = node.right //rekurencyjna zmiana dzieci
            if parrent.left == node
            then parrent.left = node.left //rekurencyjna zmiana dzieci
            else
              depthFirstSearch(right, elem :: list, node)


    def breadthFirstSearch[A](tree: BT[A]): List[A]=

    def main(args: Array[String]): Unit =
      println(toHex(31))
      println(changeBase(128, 2))
      val tree = createNLevelTree(2)
      println(tree)
      println(treeProduct(tree))
  }