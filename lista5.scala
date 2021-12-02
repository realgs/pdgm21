import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random
object lista5 {
  //2
  def toBase(base: Int)(number: Int): List[Int] =
    if base <= 1 then throw new Exception("base must be greater than 1")
    else if number < 0 then throw new Exception("number must be non-negative")
    else if number == 0 then List(0)
    else
      def toBaseRec(acc: List[Int])(number: Int): List[Int] =
        if number == 0 then acc
        else toBaseRec(number % base :: acc)(number / base)

      toBaseRec(Nil)(number)

  //1
  val toBase16 = toBase(16)

  //3
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[+A](elem: A, children: List[Tree[A]]) extends Tree[A]

  def binaryTreeOfDepth(depth: Int): Tree[Double] =
    val random = new Random()
    def BTDepthRec(depth: Int): Tree[Double] =
      if depth == 0 then Empty
      else Node(random.nextDouble(), List(BTDepthRec(depth - 1), BTDepthRec(depth - 1)))

    BTDepthRec(depth)

  //4

  def foldLeft[A, B](list: List[A])(firstValue: B)(callback: (B, A) => B): B =
    def foldRec(acc: B, list: List[A]): B =
      list match
        case Nil => acc
        case hd :: tl => foldRec(callback(acc, hd), tl)

    foldRec(firstValue, list)

  def foldPreOrderTree[A, B](tree: Tree[A])(firstValue: B)(callback: (B, A) => B): B =
    def traverseRec(acc: B, node: Tree[A]): B =
      node match
        case Empty => acc
        case Node(elem, children) => foldLeft(children)(callback(acc, elem))(traverseRec)

    traverseRec(firstValue, tree)

  def multiplyTree(tree: Tree[Double]) = foldPreOrderTree(tree)(1.0)((acc, x) => acc * x)

  //5
  def DFremoveDuplicatesFromTree[A](tree: Tree[A]) =
    val nodeSet = new mutable.HashSet[A]()

    def rmDuplicateChildrenRec(nodes: List[Tree[A]]): List[Tree[A]] =
      nodes match
        case Nil => Nil
        case Empty :: tl => rmDuplicateChildrenRec(tl)
        case Node(elem, children) :: tl =>
          if nodeSet.contains(elem) then
            rmDuplicateChildrenRec(children ::: tl)
          else
            nodeSet.add(elem)
            Node(elem, rmDuplicateChildrenRec(children)) :: rmDuplicateChildrenRec(tl)

    tree match
      case Empty => Empty
      case Node(elem, children) =>
        nodeSet.add(elem)
        Node(elem, rmDuplicateChildrenRec(children))

  def BFremoveDuplicatesFromTree[A](tree: Tree[A]) =
    val nodeSet = new mutable.HashSet[A]()

    def rmDuplicateChildrenRec(nodes: List[Tree[A]]): List[Tree[A]] =
      nodes match
        case Nil => Nil
        case Empty :: tl => rmDuplicateChildrenRec(tl)
        case Node(elem, children) :: tl =>
          if nodeSet.contains(elem) then
            rmDuplicateChildrenRec(children ::: tl)
          else
            nodeSet.add(elem)
            val t = rmDuplicateChildrenRec(tl)
            Node(elem, rmDuplicateChildrenRec(children)) :: t

    tree match
      case Empty => Empty
      case Node(elem, children) =>
        nodeSet.add(elem)
        Node(elem, rmDuplicateChildrenRec(children))


  def main(args: Array[String]): Unit =
    println(toBase16(31))
    println(binaryTreeOfDepth(3))
    println(multiplyTree(binaryTreeOfDepth(3)))

    def testTreeOfDepth(first: Int)(depth: Int): Tree[Double] =
      if depth == 0 then Empty
      else Node(first, List(testTreeOfDepth(2 * first)(depth - 1), testTreeOfDepth(2 * first + 1)(depth - 1)))


    println(multiplyTree(testTreeOfDepth(1)(2)))
    println(multiplyTree(testTreeOfDepth(1)(3))) // == 5,040 == 7!
    val tree1 = Node(12, List(
      Node(15, List(
        Node(99, Nil),
        Node(99, Nil)
      )),
      Node(15, List(
        Node(99, Nil),
          Node(99, Nil)
      )),
      Node(99, List(
        Node(35, Nil),
        Node(35, Nil)
      ))
    ))
    println(DFremoveDuplicatesFromTree(tree1))
    println(BFremoveDuplicatesFromTree(tree1))
}
