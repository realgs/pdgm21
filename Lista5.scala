import scala.annotation.tailrec
object Lista5 {

  def decHexConverter(num: Int): List[Int] =
    if num == 0 then List(0)
    else
      @tailrec
      def decHexConverterIter(num: Int, acc: List[Int]): List[Int] =
        if num == 0 then acc
        else
          if num < 0 then decHexConverterIter((-1)*num / 16, (-1) :: num % 16 :: acc)
          else decHexConverterIter(num / 16, num % 16 :: acc)
      decHexConverterIter(num, List())

  def decAnyConverter(num: Int, base: Int): List[Int] =
    if base <= 1 then throw new Exception("Wrong base passed as argument! base>1!")
    if num == 0 then List(0)
    @tailrec
    def decAnyConverterIter(num: Int, acc: List[Int]): List[Int] =
      if num == 0 then acc
      else
        if num < 0 then decAnyConverterIter((-1)*num / base, (-1) :: num % base :: acc)
        else decAnyConverterIter(num / base, num % base :: acc)
    decAnyConverterIter(num, Nil)


  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  val rand = scala.util.Random

  def generateTree(depth: Int): BT[Double] =
    if depth == 0 then Node(rand.nextDouble(), Empty, Empty)
    else Node(rand.nextDouble(), generateTree(depth-1), generateTree(depth-1))

  def breadthProduct(tree: BT[Double]): Double =
    @tailrec
    def breadthProductIter(childQueue: List[BT[Double]], acc: Double): Double =
      childQueue match
        case Nil => acc
        case Empty :: t => breadthProductIter(t, acc)
        case Node(elem, left, right)::t => breadthProductIter (t ::: List(left, right), elem * acc)
    breadthProductIter (List(tree), 1)

  //Zadanie 5

  @tailrec
  def contain[A](elem: A, list: List[A]): Boolean = {
    list match
      case Nil => false
      case h :: t =>
        if elem == list.head then true
        else contain(elem, list.tail)
  }

  def TreeFromList[A](list: List[A]): BT[A] =
    list match
      case Nil => Empty
      case head :: tail =>
        val split = tail.splitAt(tail.length / 2)
        Node(head, TreeFromList(split._1), TreeFromList(split._2))

  def DeleteDupBS[A](tree: BT[A]): BT[A] = {
    def DeleteDupBSIter[A](queue: List[BT[A]], list: List[A]): List[A] = queue match
      case Nil => Nil
      case Empty :: t => DeleteDupBSIter(t, list)
      case Node(elem, left, right) :: t =>
        if (contain(elem, list)) then DeleteDupBSIter(t ::: List(left, right), list)
        else elem :: DeleteDupBSIter(t ::: List(left, right), elem :: list)
    TreeFromList(DeleteDupBSIter(List(tree), Nil))
  }

  def DeleteDupDS[A](tree: BT[A]): BT[A] = {
    def DeleteDupDSIter[A](stack: List[BT[A]], list: List[A]): List[A] = stack match
      case Nil => Nil
      case Empty :: t => DeleteDupDSIter(t, list)
      case Node(elem, left, right) :: t =>
        if (contain(elem, list)) then DeleteDupDSIter(left::right::t, list)
        else elem :: DeleteDupDSIter(left::right::t, elem :: list)
    TreeFromList(DeleteDupDSIter(List(tree), Nil))
  }


  def main(args: Array[String]): Unit = {
    println(decHexConverter(-74))
    println(decHexConverter(0))
    println(decAnyConverter(-74, 16))
    println(decAnyConverter(0, 16))
    //println(decAnyConverter(74, 1))
    //println(decAnyConverter(74, 0))
    println(generateTree(5))
    println(breadthProduct(generateTree(3)))
    val t1 = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(2, Node(5, Empty, Node(2, Empty, Empty)), Empty))
    val t2 = DeleteDupBS(t1)
    val t3 = DeleteDupDS(t1)
    println(t2)
    println(t3)
  }
}
