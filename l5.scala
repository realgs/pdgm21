object l5 {

  // Task 1
  def decimalToHex(x: Int): List[Int] =
    def inner(acc: Int)(list: List[Int]): List[Int] =
      if acc >= 16 then inner(acc / 16)(acc % 16 :: list)
      else acc :: list

    inner(x)(List())

  // Task 2
  def decimaltoNumberSystem(x: Int, base: Int): List[Int] =
    def inner(acc: Int)(list: List[Int]): List[Int] =
      if acc >= base then inner(acc / base)(acc % base :: list)
      else acc :: list

    inner(x)(List())

  // Task 3
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def randomTree(depth: Int): BT[Double] =
    if depth == 0 then Empty
    else Node(Math.random(), randomTree(depth - 1), randomTree(depth - 1))

  // Task 4
  def treeNodesProduct(tree: BT[Double]): Double =
    tree match
      case Node(elem, left, right) => elem * treeNodesProduct(left) * treeNodesProduct(right)
      case Empty => 1

  def main(args: Array[String]): Unit = {
    // Task 1
    println("DecimalToHex method test:")
    println(decimalToHex(31) == List(1, 15))
    println(decimalToHex(16) == List(1, 0))
    println(decimalToHex(0) == List(0))
    println(decimalToHex(2560) == List(10, 0, 0))

    // Task 2
    println("DecimaltoNumberSystem method test:")
    println(decimaltoNumberSystem(31, 8) == List(3, 7))
    println(decimaltoNumberSystem(1025, 2) == List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
    println(decimaltoNumberSystem(0, 5) == List(0))
    println(decimaltoNumberSystem(2560, 10) == List(2, 5, 6, 0))

    // Task 4
    println(treeNodesProduct(randomTree(3)))
    println(treeNodesProduct(Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Empty, Empty))) == 120)
  }
}
