import scala.annotation.tailrec
import scala.util.Random

object l5 {
  sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def random = Random()

  //2
  def getDigitsList(number: Int, base: Int): List[Int] =
    @tailrec
    def getDigitsListHelper(toBeDivided: Int, resultList: List[Int]): List[Int] =
      if toBeDivided == 0 then (if resultList == Nil then List(0) else resultList)
      else getDigitsListHelper(toBeDivided / base, toBeDivided % base :: resultList)
    getDigitsListHelper(number, Nil)

  //3
  def buildTree(depth: Int): BT[Float] =
    def buildTreeHelper(currentLevel: Int): BT[Float] =
      if currentLevel == depth then Empty
      else Node(random.nextFloat(), buildTreeHelper(currentLevel + 1), buildTreeHelper(currentLevel + 1))
    buildTreeHelper(0)

  //4
  def nodesProduct(tree: BT[Float]): Float =
    @tailrec
    def nodesProductHelper(toBeMultiplied: List[BT[Float]], product: Float): Float =
      toBeMultiplied match
        case (Node(value, left, right)) :: listTail => nodesProductHelper(left :: right :: listTail, value * product)
        case Empty :: listTail => nodesProductHelper(listTail, product)
        case Nil => product
    nodesProductHelper(List(tree), 1f)

  //5
  def removeDuplicatesDepth[A](tree: BT[A]): BT[A] =
    def removeDuplicatesDepthHelper[A](underCheck: BT[A], visited: List[A]): BT[A] =
      underCheck match
        case Empty => println("Empty"); Empty
        case Node(value, left, right) => println(value); Node(value, (if(left == Empty || visited.contains(left.asInstanceOf[Node[A]].elem)) then Empty else removeDuplicatesDepthHelper(left, value :: visited)), (if(right == Empty || visited.contains(right.asInstanceOf[Node[A]].elem)) then Empty else removeDuplicatesDepthHelper(right, value :: visited)))
    removeDuplicatesDepthHelper(tree, Nil)

  def main(args: Array[String]): Unit = {}
}
