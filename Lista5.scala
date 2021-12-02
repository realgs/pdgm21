object Lista5 {

  import scala.annotation.tailrec
  import scala.util.Random

  //Zad 1.

  def decimalToHexadecimal(number: Double): List[Int] =
    @tailrec
    def decimalToHexadecimalHelper(number: Double, positionValue: Double, result: List[Int]): List[Int] =
      if number == 0 then result
      else
        if positionValue * 16 > number then
          if positionValue == (1.0 / 16.0) then decimalToHexadecimalHelper((number % positionValue), (positionValue / 16), (number / positionValue).toInt :: -1 :: result)
          else
            decimalToHexadecimalHelper((number % positionValue), (positionValue / 16), (number / positionValue).toInt :: result)
        else decimalToHexadecimalHelper(number, (positionValue * 16), result)

    if number >= 0 then (decimalToHexadecimalHelper(number, 1, Nil)).reverse
    else -1::(decimalToHexadecimalHelper( -1 * number, 1, Nil)).reverse


  //Zad 2.

  def decimalToOther(number: Double, base: Int): List[Int] =
    @tailrec
    def decimalToHexadecimalHelper(number: Double, positionValue: Double, result: List[Int]): List[Int] =
      if number == 0 then result
      else
        if positionValue * base > number then
          if positionValue == (1.0 / base) then decimalToHexadecimalHelper((number % positionValue), (positionValue/ base), (number / positionValue).toInt :: -1 :: result)
          else
            decimalToHexadecimalHelper((number % positionValue), (positionValue / base), (number / positionValue).toInt :: result)
        else decimalToHexadecimalHelper(number, (positionValue * base), result)

    if base <= 1 then List()
    else
      if number >= 0 then (decimalToHexadecimalHelper(number, 1, Nil)).reverse
      else -1::(decimalToHexadecimalHelper( -1 * number, 1, Nil)).reverse


  //Zad 3.

  sealed trait BT[A]
  case class Empty[A]() extends BT[A]
  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def makeTree(high: Int): BT[Double] =
      if high <= 0 then Empty()
      else
        Node(Random().nextDouble(), makeTree(high-1), makeTree(high-1))

  //Zad 4.

  def treeCount(tree: BT[Double] ): Double =
    @tailrec
    def treeCountHelper(heap: List[BT[Double]], result: Double): Double =
      if heap == Nil then result
      else
      heap.head match
      case Node(value, left, right) => treeCountHelper(left :: right :: heap.tail, value * result)
      case Empty() => treeCountHelper(heap.tail, result)

    if tree == Empty() then 0
    else treeCountHelper(List(tree), 1)


  //Zad 5.

  def treeDFSDeleteDuplicats[A](tree: BT[A]): List[BT[A]] =
    @tailrec
    def treeDepthDeleteDuplicatsHelper(heap: List[BT[A]], foundValues: List[A], result: List[BT[A]]): List[BT[A]] =
      if heap == Nil then result
      else
        heap.head match
          case Node(value, left, right) =>
            if foundValues.contains(value) then treeDepthDeleteDuplicatsHelper(left :: right :: heap.tail, foundValues, result)
            else treeDepthDeleteDuplicatsHelper(left :: right :: heap.tail, value :: foundValues, heap.head::result)
          case Empty() => treeDepthDeleteDuplicatsHelper(heap.tail, foundValues, result)

    treeDepthDeleteDuplicatsHelper(List(tree), Nil, Nil).reverse

  def treeBFSDeleteDuplicats[A](tree: BT[A]): List[BT[A]] =
    @tailrec
    def treeBFSDeleteDuplicatsHelper(queue: List[BT[A]], foundValues: List[A], result: List[BT[A]]): List[BT[A]] =
      if queue == Nil then result
      else
        queue.head match
          case Node(value, left, right) =>
            if foundValues.contains(value) then treeBFSDeleteDuplicatsHelper( queue.tail ::: List(left, right) , foundValues, result)
            else treeBFSDeleteDuplicatsHelper(queue.tail ::: List(left, right), value :: foundValues, queue.head::result)
          case Empty() => treeBFSDeleteDuplicatsHelper(queue.tail, foundValues, result)

    treeBFSDeleteDuplicatsHelper(List(tree), Nil, Nil).reverse

  def main(args: Array[String]): Unit =
  {
    println(decimalToHexadecimal(-24.52))
    println(decimalToHexadecimal(0.15))
    println(decimalToHexadecimal(0))
    println(decimalToHexadecimal(-13))
    println(decimalToHexadecimal(256))
    println()
    println(decimalToOther(24.6, 2))
    println(decimalToOther(0, 12))
    println(decimalToOther(256, 8))

    println(makeTree(3))
    println(treeCount(makeTree(3)))
    println(makeTree(0))
    println(treeCount(makeTree(0)))
    println(makeTree(4))
    val tree = makeTree(4)
    println(treeDFSDeleteDuplicats(tree))
    println(treeBFSDeleteDuplicats(tree))
  }
}
