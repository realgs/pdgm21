import scala.util.Random

object l5 {

  def decToHex(value: Int): List[Int] =
    def decToHexInner(div: Int, hexResult: List[Int]): List[Int] = //tailrec
      if div <= 0 then hexResult
      else decToHexInner((div/16) , (div%16) :: hexResult)
    decToHexInner(value, Nil)


  def decToAny(value: Int, base: Int): List[Int] =
    def decToAnyInner(div: Int, result: List[Int]): List[Int] = //tailrec
      if div <= 0 then result
      else decToAnyInner((div/base) , (div%base) :: result)
    decToAnyInner(value, Nil)

  //tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateTree(levels: Int): BT[Float] =
    def generateTreeInner(levelsToGenerate: Int, resultTree: BT[Float]): BT[Float] = //tailrec
      levelsToGenerate match
        case -1 => resultTree
        case _ => Node(Random.nextFloat(), generateTreeInner(levelsToGenerate-1, Empty), generateTreeInner(levelsToGenerate-1, Empty))
    generateTreeInner(levels, Empty)

  def multiple(tree: BT[Float]): Float =
    def multipleInner(stack: List[BT[Float]], mul: Float): Float =
      if stack == Nil then mul
      else stack.head match
        case Empty => multipleInner(stack.tail, mul)
        case Node(v,l,r) => multipleInner(l :: r :: stack.tail, mul*v)
    multipleInner(List(tree), 1)

  def breadthBTree[A](tree: BT[A]): List[A] =
    def breadthBTreeInner[A](queue: List[BT[A]], result: List[A]): List[A] =
      if queue == Nil then result
      else queue.head match
        case Empty => breadthBTreeInner(queue.tail, result)
        case Node(v,l,r) => breadthBTreeInner(queue.tail ::: List(l,r), v :: result)
    breadthBTreeInner(List(tree), Nil).reverse

  def depthBTree[A](tree: BT[A]): List[A] =
    def depthBTreeInner[A](stack: List[BT[A]], result: List[A]): List[A] =
      if stack == Nil then result
      else stack.head match
        case Empty => depthBTreeInner(stack.tail, result)
        case Node(v,l,r) => depthBTreeInner(l :: r :: stack.tail, v :: result)
    depthBTreeInner(List(tree), Nil).reverse


  //ex 5

  def contains[A](value: A, list: List[A]): Boolean =
    list match
      case h :: t => if value == h then true
                      else contains(value, t)
      case Nil => false

  def removeDuplicatesBreadth[A](tree: BT[A]): BT[A] =
    def removeDuplicatesBreadthIter[A](queue: List[BT[A]], visited: List[A]): List[A] =
      queue match
        case Nil        => visited
        case Empty :: t => removeDuplicatesBreadthIter(t, visited)
        case Node(v, l, r) :: t => if contains(v, visited) then removeDuplicatesBreadthIter(t ++ List(l, r), visited)
                                    else removeDuplicatesBreadthIter(t ++ List(l, r), v :: visited)
    createTree(removeDuplicatesBreadthIter(List(tree), Nil).reverse)


  def removeDuplicatesDepth[A](tree: BT[A]): BT[A] =
    def removeDuplicatesDepthIter[A](stack: List[BT[A]], visited: List[A]): List[A] =
      stack match
        case Nil        => visited
        case Empty :: t => removeDuplicatesDepthIter(t, visited)
        case Node(v, l, r) :: t => if contains(v, visited) then removeDuplicatesDepthIter(l :: r :: t, visited)
                                    else removeDuplicatesDepthIter(l :: r :: t, v :: visited)
    createTree(removeDuplicatesDepthIter(List(tree), Nil).reverse)


  def createTree[A](list: List[A]): BT[A] =
    list match
      case Nil => Empty
      case h :: t => val div = t.splitAt(t.length / 2)
        Node(h, createTree(div._1), createTree(div._2))


  def main(args: Array[String]): Unit = {
    println("Dec to hex")
    println(decToHex(31))
    println(decToHex(32))

    println("Dec to any")
    println(decToAny(31, 16))
    println(decToAny(32, 16))
    println(decToAny(64, 2))
    println(decToAny(64, 8))
    println(decToAny(127, 10))
    println(decToAny(127, 2))

    println("Generate tree")
    println(generateTree(3))
    println(generateTree(1))
    println(generateTree(0))

    println("Tree for multiply function test")
    val testTree = generateTree(3)
    println(testTree)
    println("Multiply values in a tree =")
    println(multiple(testTree)) //aprox!
    println("Breadth search tree")
    println(breadthBTree(testTree))
    println("Depth search tree")
    println(depthBTree(testTree))

    val tree = Node(1,
      Node(2,
        Node(4,
          Empty,
          Empty
        ),
        Empty
      ),
      Node(4,
        Node(4,
          Empty,
          Node(6,
            Empty,
            Empty
          )
        ),
        Empty
      )
    )
    println("\nBefore deleting duplicates - BST\n")
    println(breadthBTree(tree))
    println("\nAfter deleting duplicates - BST\n")
    println(removeDuplicatesBreadth(tree))
  }
}
