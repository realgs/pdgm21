import java.util.InputMismatchException
import scala.annotation.tailrec

object lista5 {

  // returns reversed list
  def myReverse[A](list: List[A]): List[A] = {
    // time complexity: n, where n is list length
    @tailrec
    def myReverseHelper(list: List[A], result: List[A]): List[A] = {
      if list.isEmpty then result
      else myReverseHelper(list.tail, list.head :: result)
    }

    myReverseHelper(list, List())
  }

  // worse time complexity than decToHex2
  def decToHex1(decNum: Int): List[Int] = {
    // time complexity:  log16(n) * [ log16(n) * (log16(n) - 1) / 2 ]
    // memory complexity: log16(n)
    // where n is decimal number from input

    if (decNum < 0) then decToHex1(-decNum);
    else if (decNum < 16) then List(decNum)
    else decToHex1(decNum / 16) ::: List(decNum % 16)

  }

  // better time complexity, comparable memory complexity to decToHex1
  def decToHex2(decNum: Int): List[Int] = {
    // time complexity: log16(n)
    // memory complexity: 1 + log16(n)
    // where n is decimal number from input
    @tailrec
    def decToHecIter(d: Int, reversedHex: List[Int]): List[Int] = {
      if (d < 16) then d :: reversedHex
      else decToHecIter(d / 16, (d % 16) :: reversedHex)
    }

    decToHecIter(Math.abs(decNum), List())
  }

  // worse time complexity than decToAnything2
  def decToAnything1(decNum: Int, base: Int): List[Int] = {
    // time complexity:  logx(n) * [ logx(n) * (logx(n) - 1) / 2 ]
    // memory complexity: 2 * logx(n)
    // where x is base of new numerical system and n is decimal number from input

    if base <= 0 then throw new InputMismatchException()

    if (decNum < 0) then decToAnything1(-decNum, base)
    else if (decNum < base) then List(decNum)
    else decToAnything1(decNum / base, base) ::: List(decNum % base)

  }

  // better time complexity, comparable memory complexity than decToHex1
  def decToAnything2(decNum: Int, base: Int): List[Int] = {
    // time complexity: logx(n)
    // memory complexity: 1 + 1 + logx(n)
    // where x is base of new numerical system and n is decimal number from input

    if base <= 0 then throw new InputMismatchException()

    @tailrec
    def iter(d: Int, result: List[Int]): List[Int] = {
      if (d < base) d :: result
      else iter(d / base, (d % base) :: result)
    }

    iter(Math.abs(decNum), List())
  }

  // trait descriptiong binary trees
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  // returns a binary tree of depth N
  def createTree(depth: Double): BT[Double] = {
    // time complexity: 2^N
    // memory complexity: 2^N
    // where N is tree depth from input

    if depth < 0 then throw new InputMismatchException("depth of tree cannot be negative!")

    val random = new scala.util.Random()

    if depth == 0 then Empty
    else Node(random.nextDouble(), createTree(depth - 1), createTree(depth - 1))

  }

  // returns product of all elements in a binary tree
  def product(tree: BT[Double]): Double = {
    // time complexity: n
    // memory complexity: n * current_tree_size = n * 0.5 * n * (n-1)
    // where n = [2 ^ (N + 1) - 1] is number of elements in the tree from input and N is depth of said tree

    tree match
      case Empty => 1
      case Node(x, left, right) => x * product(left) * product(right)
  }

  // traverses the tree using DFS and applies function from input to each element
  def DFS[A](tree: BT[A], operation: (BT[A] => BT[A])): BT[A] = {
    // time complexity: n * complexity_of_operation()
    // memory complexity: n
    // where n = [2 ^ (N + 1) - 1] is number of elements in the tree from input and N is depth of said tree

    tree match
      case Empty => tree
      case Node(x, left, right) => operation(Node(x, DFS(left, operation), DFS(right, operation)))
  }

  // removes root of the finary tree from input
  def deleteDeepest[A](tree: BT[A]): BT[A] = {
    if tree == Empty then Empty
    else
      val Node(c, _, _) = tree
      var candidate = c

      def iter(bt: BT[A]): BT[A] = {
        bt match
          case Empty => Empty
          case Node(x, Empty, Empty) => {
            candidate = x
            Empty
          }
          case Node(x, Empty, right) => Node(x, Empty, iter(right))
          case Node(x, left, right) => Node(x, iter(left), right)
      }

      val Node(elem, left, right) = iter(tree) // tree with deleted leaf
      Node(candidate, left, right) // tree with root replaced with deleted leaf
  }

  // returns binary tree from input with duplicates removed
  def removeDuplicates[A](tree: BT[A], traversalMethod: (BT[A], (BT[A] => BT[A])) => BT[A]): BT[A] = {
    // time complexity:
    // memory complexity:

    // stores elements that have already appeared in the tree at least once
    var visited = Set[A]()

    traversalMethod(tree,
      (bt: BT[A]) => (
        bt match
          case Empty => Empty
          case Node(x, left, right) =>
            if visited.contains(x) then {
              //print(s"removing [$x]; ")

              if left == Empty && right == Empty then Empty
              else if left == Empty then right
              else if right == Empty then left
              else deleteDeepest(bt)
            }
            else {
              //print(s"visiting [$x]; ")
              visited = visited + x
              bt
            }
        )) // end traversalMethod


  }

  // traverses the tree with DFS and removes duplicates
  def removeDuplicatesDepth[A](tree: BT[A]) = removeDuplicates(tree, DFS)


  def main(args: Array[String]) = {

    // tests
    
    println("TASK 1: decimal to hexadecimal conversion")
    println(decToHex1(31) == List(1, 15))
    println(decToHex2(31) == List(1, 15))
    println(decToHex1(13) == List(13))
    println(decToHex2(13) == List(13))
    println(decToHex1(-56) == List(3, 8))
    println(decToHex2(-56) == List(3, 8))

    println("TASK 2: decimal to any positional numeral system conversion")
    println(decToAnything1(31, 16) == List(1, 15))
    println(decToAnything2(31, 16) == List(1, 15))
    println(decToAnything1(12, 2) == List(1, 1, 0, 0))
    println(decToAnything2(12, 2) == List(1, 1, 0, 0))
    println(decToAnything1(-12, 8) == List(1, 4))
    println(decToAnything2(-12, 8) == List(1, 4))

    println("TASK 3: create a binary tree on depth N")
    println(createTree(0) == Empty)
    println(
      createTree(1) match
        case Node(x: Double, Empty, Empty) => if (x < 1 && x > 0) then true else false
        case _ => false
    )
    println(
      createTree(2) match
        case Node(x, Node(y: Double, Empty, Empty), Node(z: Double, Empty, Empty)) => true
        case _ => false
    )

    println("TASK 4: calculate product of all elements in a binary tree")
    println(product(Empty) == 1.0)
    println(product(Node(.1, Empty, Empty)) == .1)
    println(product(Node(.3, Node(.4, Empty, Empty), Node(-.5, Empty, Empty))) == -.06)

    println("TASK 5: remove duplicates from a binary tree")
    println("TASK 5.1: using DFS")
    println(removeDuplicatesDepth(Node(.3, Node(.4, Empty, Empty), Node(.4, Empty, Empty)))
      == Node(.3, Node(.4, Empty, Empty), Empty))

    println(removeDuplicatesDepth(Node(.4, Node(.1, Empty, Empty), Node(.4, Empty, Empty)))
      == Node(.1, Empty, Node(.4, Empty, Empty)))

    println(removeDuplicatesDepth(Node(.1,
      Node(.7, Node(.4, Empty, Empty), Node(.7, Empty, Empty)),
      Node(.3, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))
      == Node(0.1, Node(0.4, Empty, Node(0.7, Empty, Empty)), Node(0.3, Node(0.6, Empty, Empty), Empty)))

    println(removeDuplicatesDepth(Node(.4, Empty, Empty)) == Node(.4, Empty, Empty))

    println(removeDuplicatesDepth(Empty) == Empty)

    
  }


}
