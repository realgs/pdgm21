import scala.annotation.tailrec

object L5 {

  //zadanie 1
  def decimalToHeximal(number: Int): List[Int] =
    @tailrec
    def helper(number: Int, list: List[Int]): List[Int] =
      if number == 0 && list == Nil then List(0)
      else if number == 0 then list
      else helper(number/16, (number%16) :: list)
    helper(number, Nil)

  //+5 pkt
  def decimalToAnyOther(number: Int, systemNr: Int): List[Int] =
    @tailrec
    def helper(number: Int, systemNr: Int, list: List[Int]): List[Int] =
      if number == 0 && list == Nil then List(0)
      else if number == 0 then list
      else helper(number/systemNr, systemNr, (number%systemNr) :: list)
    helper(number, systemNr, Nil)

  //define binary tree
  sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //random number generator
  val rnd = scala.util.Random

  //zadanie 3
  def generateTree(depth: Int): BT[Double] =
    if depth == 0 then Empty
    else Node(rnd.nextDouble(), generateTree(depth - 1), generateTree(depth - 1))

  //zadanie 4
  def productOfTreeElements(tree: BT[Double]): Double =
    tree match
      case Empty => 1
      case Node(value, lt, rt) => value * productOfTreeElements(lt) * productOfTreeElements(rt)


  def main(args: Array[String]): Unit = {
    println(decimalToHeximal(31) == List(1, 15))
    println(decimalToHeximal(0) == List(0))
    println(decimalToHeximal(2137) == List(8, 5, 9))
    println()
    println(decimalToAnyOther(31, 16) == List(1, 15))
    println(decimalToAnyOther(0, 8) == List(0))
    println(decimalToAnyOther(2137, 10) == List(2, 1, 3, 7))
    println(decimalToAnyOther(73, 2) == List(1, 0, 0, 1, 0, 0, 1))
    println()
    val tree = generateTree(3)
    println(productOfTreeElements(tree))
    val tree2 = Node(0.3, Node(0.4, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty)), Node(0.4, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty)))
    println(productOfTreeElements(tree2) == 0.003)
  }
}
