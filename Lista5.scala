import scala.annotation.tailrec
import scala.util.Random
object Lista5 {
  //Zadanie 1
  //rekursja ogonowa, nie używanie reverse
  def convertionDecToHex(number: Int): List[Int]=
    if number >= 0 then
      @tailrec
      def helper(number: Int, convertedList: List[Int]): List[Int]=
        if number == 0 then convertedList
        else helper(number / 16, (number % 16)::convertedList)
      helper(number / 16, List(number % 16))
    else Nil


  //Zadanie 2
  def convertionDecToSystem(number: Int, system: Int): List[Int]=
    if number >= 0 && system > 1 && system != 10 then
      @tailrec
      def helper(number: Int, convertedList: List[Int]): List[Int]=
        if number == 0 then convertedList
        else helper(number / system, (number % system)::convertedList)
      helper(number / system, List(number % system))
    else Nil


  //Zadanie 3
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = Random()
  def generateTree(depth: Int): BT[Double] =
    if depth >= 0 then
      depth match
        case 0 => Empty
        case _ => Node(random.nextDouble(), generateTree(depth - 1), generateTree(depth - 1))
    else Empty


  //Zadanie 4
  def ElementsOfTree(tree: BT[Double]): Double =
    def helper(node: BT[Double]): Double =
      node match
        case Node(value, leftSubtree, rightSubtree) => value * helper(leftSubtree) * helper(rightSubtree)
        case Empty => 1
    if tree == Empty then 0
    else helper(tree)

  //Zadanie 5 niedk.
  def breadthBT[A](bt: BT[A]) = {
    def helpFun[A](queue: List[BT[A]]): List[A] = queue match
      case Nil => Nil
      case Empty :: tail => helpFun(tail)
      case Node(value, leftSubtree, rightSubtree) :: tail => value :: helpFun(tail ::: List(leftSubtree, rightSubtree))

    helpFun(List(bt))
  }

  def depthBT[A](bt: BT[A]) = {
    def helpFun[A](stack: List[BT[A]]): List[A] = stack match
      case Nil => Nil
      case Empty :: tail => helpFun(tail)
      case Node(value, leftSubtree, rightSubtree) :: tail => value :: helpFun(leftSubtree :: rightSubtree :: tail)

    helpFun(List(bt))
  }
  @tailrec
  def find[A](valueList: List[A], index: Int):A= {
    (valueList, index) match
      case (head :: _, 0) => head
      case (_ :: tail, _) => find(tail, index-1)
  }

  def listLength[A](list: List[A]): Int =
    @tailrec
    def helper(list: List[A], length: Int): Int =
      list match
        case Nil => length
        case head :: tail => helper(tail, length + 1)
    helper(list, 0)

  def main(aargs: Array[String]): Unit = {
    //zadanie 1
    println(convertionDecToHex(0) == List(0))
    println(convertionDecToHex(-5) == List())
    println(convertionDecToHex(4) == List(4))
    println(convertionDecToHex(15) == List(15))
    println(convertionDecToHex(31) == List(1, 15))
    println(convertionDecToHex(33) == List(2, 1))
    println(convertionDecToHex(1000) == List(3, 14, 8))

    //zadanie 2
    println(convertionDecToSystem(-9, 3) == List())
    println(convertionDecToSystem(9, -3) == List())
    println(convertionDecToSystem(10, 10) == List())
    println(convertionDecToSystem(9, 3) == List(1, 0, 0))
    println(convertionDecToSystem(10, 2) == List(1, 0, 1, 0))
    println(convertionDecToSystem(67, 2) == List(1, 0, 0, 0, 0, 1, 1))
    println(convertionDecToSystem(80, 8) == List(1, 2, 0))
    println(convertionDecToSystem(31, 16) == List(1, 15))

    //zadanie 3,4
    val t0= generateTree(0)
    val t1 = generateTree(1)
    val t2 = generateTree(2)
    val t3 = generateTree(3)

    println(t0)
    println(t1)
    println(t2)
    println(t3)

    println(ElementsOfTree(Node(0.2, Node(0.1, Empty, Empty), Node(0.2, Empty, Empty))))
    println(ElementsOfTree(Node(0.3,Node(0.3,Node(0.1,Empty,Empty),Node(0.2,Empty,Empty)),Node(0.1,Node(0.1,Empty,Empty),Node(0.1,Empty,Empty)))))
    println(ElementsOfTree(t0))
    println(ElementsOfTree(t1))
    println(ElementsOfTree(t2))
    println(ElementsOfTree(t3))

    //zadanie 5
    println(breadthBT(t0))
    println(breadthBT(t1))
    println(breadthBT(t2))
    println(breadthBT(t3))

    println(depthBT(t0))
    println(depthBT(t1))
    println(depthBT(t2))
    println(depthBT(t3))

    println(find(depthBT(t2), 2))
    println(listLength(depthBT(t2)))
  }
}
