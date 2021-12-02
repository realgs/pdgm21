import scala.annotation.tailrec

object lab5 {

  def toHexCalculator(number: Int): List[Int] =
    if number < 0 then Nil
    else if number == 0 then List(0)
    else
      @tailrec
      def hexCalculatorHelper(number: Int, result: List[Int]) : List[Int] =
        if number == 0 then result
        else if number == number % 16 then hexCalculatorHelper(0, (number % 16) :: result)
        else  hexCalculatorHelper(number/16, number%16 :: result)
      hexCalculatorHelper(number, List())

  def toAnyCalculator(number: Int, system_number: Int): List[Int] =
    if number<0 then Nil
    else if number == 0 then List(0)
    else
      @tailrec
      def toAnyCalculatorHelper(number: Int, system_number: Int, result: List[Int]): List[Int] =
        if number == 0 then result
        else if number == number % system_number then toAnyCalculatorHelper(0, system_number, (number % system_number) :: result)
        else  toAnyCalculatorHelper(number/system_number, system_number, number%system_number :: result)
      toAnyCalculatorHelper(number, system_number, List())

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


  def buildTree (n: Int): BT[Double]=
    if n==0 then Empty
    else Node(math.random(), buildTree(n-1), buildTree(n-1))


  def treeProduct(tree: BT[Double]): Double =
    @tailrec
    def treeProductHelper(queue: List[BT[Double]], acc: Double): Double =
      queue match {
        case Nil => acc
        case Node(elem, left, right) :: t => treeProductHelper(t ::: List(left, right), acc*elem)
        case Empty :: t => treeProductHelper(t, acc)
      }
    treeProductHelper(List(tree), 1)





  def main(args: Array[String]): Unit ={
    val t = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(3, Empty, Empty))
    println(t)

    println(buildTree(3))

    println(treeProduct(buildTree(3)))

    println(toHexCalculator(31))
    println(toHexCalculator(0))

    println(toAnyCalculator(31, 2))
    println(toAnyCalculator(31, 16))
    println(toAnyCalculator(32, 8))

  }
}
