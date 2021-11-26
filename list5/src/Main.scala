import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit ={
    println(decimalToHex(1128));
    println(decimalToHex(256));
    println(decimalToHex(921));
    println(decimalToHex(188));
    println(decimalToHex(590));
    println(decimalToOther(10, 2));
    println(decimalToOther(1458, 2));
    println(decimalToOther(10, 4));
    println(decimalToOther(1000, 8));

    val tree = generateTreeOfDepth(3);

    println(breadthBT(tree));
    println(prodOfTree(tree));
    println(breadthBT(generateTreeOfDepth(2)));
    println(breadthBT(generateTreeOfDepth(1)));
    println(breadthBT(generateTreeOfDepth(0)));

  }

  sealed trait BinTree[+A]
  case object Empty extends BinTree[Nothing]
  case class Node[+A](elem: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

  def decimalToHex(number: Int): List[Int] = {
    @tailrec
    def iterator(acc: Int, list: List[Int]) : List[Int]={
    if acc > 0 then
      iterator(acc/16, (acc % 16) :: list)
    else list
    }
    iterator(number, List())
  }

  def decimalToOther(number: Int, base: Int): List[Int] = {
    @tailrec
    def iterator(acc: Int, list: List[Int]) : List[Int]={
      if acc > 0 then
        iterator(acc/base, (acc % base) :: list)
      else list
    }
    iterator(number, List())
  }

  def generateTreeOfDepth(depth: Int): BinTree[Double] ={
    val r = scala.util.Random()
    if depth == 0 then Empty
    else
      Node(r.nextInt(20), generateTreeOfDepth(depth - 1), generateTreeOfDepth(depth - 1))
  }

  def prodOfTree(tree: BinTree[Double]) : Double = {
    tree match {
      case Node(elem, Empty, Empty) => elem
      case Node(elem,left, right) => elem * prodOfTree(left) * prodOfTree(right)
    }
  }

//  def removeDuplicates[A](tree: BinTree[A]): BinTree[A] = {
//    tree match {
//      case Node()
//    }
//  }

  def breadthBT[A](tree : BinTree[A]) : List[A] = {
    @tailrec
    def queueIter(queue : List[BinTree[A]], list : List[A]) : List[A] = {
      queue match
        case Nil => list.reverse
        case Empty :: tail => queueIter(tail, list)
        case Node(v,l,r) :: tail => queueIter(tail ::: List(l,r), v :: list)
    }
    queueIter(List(tree), Nil)
  }
}
