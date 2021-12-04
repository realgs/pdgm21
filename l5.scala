import scala.util.Random

object l5 {
  // task 1
  // O(n)
  def reverseList[A](list: List[A]): List[A] =
    def reverseListIn[A](innerList: List[A], resultList: List[A]): List[A] =
      if innerList == Nil then resultList else reverseListIn(innerList.tail, innerList.head :: resultList)
    reverseListIn(list, List())

  // Ilość wywołań to najmniejsza potęga 16, która jest większa
  // od liczby, którą przekształcamy + 1, np. 31 jest mniejsze niż 16^2,
  // więc będą 3 wywołania. Do tego dochodzi jeszcze złożoność reverseList,
  // która jest liniowa względem ilości elementów
  def decToHex(decimalNumber: Int): List[Int] =
    def decToHexIn(number: Int): List[Int] =
      if number > 0 then number % 16 :: decToHexIn(number / 16) else List()
    if decimalNumber < 0 then List() else if decimalNumber == 0 then List(0)
    else reverseList(decToHexIn(decimalNumber))

  // task 2
  def decToOther(decimalNumber: Int, system: Int): List[Int] =
    def decToOtherIn(number: Int): List[Int] =
      if number > 0 then number % system :: decToOtherIn(number / system) else List()
    if decimalNumber < 0 then List() else if decimalNumber == 0 then List(0)
    else reverseList(decToOtherIn(decimalNumber))


  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](elem:A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  // task 3
  def randomTree(N: Int): BinaryTree[Float] =
    if N > 0 then Node(Random.nextFloat(), randomTree(N - 1), randomTree(N - 1)) else Empty

  // task 4
  def productOfTree(binaryTree: BinaryTree[Float]): Float =
    def productOfTreeIn(node: BinaryTree[Float]): Float =
      node match
        case Empty => 1
        case Node(e, l, r) => e * productOfTreeIn(l) * productOfTreeIn(r)
    if binaryTree == Empty then 0 else productOfTreeIn(binaryTree)

  // task 5
  def containsElement[A](list: List[A], element: A): Boolean =
    if list == Nil then false else if list.head == element then true
    else containsElement(list.tail, element)

  def removeDuplicatsBFS[A](binaryTree: BinaryTree[A]): List[A] =
    def BFS[A](list: List[BinaryTree[A]], visited: List[A]): List[A] =
      list match
        case Nil => visited
        case Empty :: t => BFS(t, visited)
        case Node(v, l, r) :: t => if containsElement(visited, v)
                                   then BFS(t ::: List(l, r), visited)
                                   else BFS(t ::: List(l, r), v :: visited)
    reverseList(BFS(List(binaryTree), List()))

  def removeDuplicatsDFS[A](binaryTree: BinaryTree[A]): List[A] =
    def DFS[A](tree: BinaryTree[A], visited: List[A]): List[A] =
      tree match
        case Empty => visited
        case Node(v, l, r) => if containsElement(visited, v)
                              then DFS(r, DFS(l, visited))
                              else DFS(r, DFS(l, v :: visited))
    reverseList(DFS(binaryTree, List()))

  //main
  def main () = {
    println("decToHex(31): " + decToHex(31))
    println("decToHex(0): " + decToHex(0))
    println("decToHex(-12): " + decToHex(-12))

    println("decToOther(31, 16): " + decToOther(31, 16))
    println("decToOther(0, 4): " + decToOther(0, 4))
    println("decToOther(32, 2): " + decToOther(32, 2))

    val tree = randomTree(2)
    println("randomTree(2): " + tree)

    println("productOfTree(tree): " + productOfTree(tree))

    val tree2 = Node(1, Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty), Node(2, Node(4, Empty, Empty), Empty))
    println("removeDuplicatsBFS(tree2): " + removeDuplicatsBFS(tree2))
    println("removeDuplicatsDFS(tree2): " + removeDuplicatsDFS(tree2))
  }
}
