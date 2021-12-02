object lista5 {

  import scala.annotation.tailrec

  //Zadanie 1
  def decimalToHex(decimalNumber: Int): List[Int] =
    decimalToAnySystem(List(decimalNumber, 16))

  //Zadanie 2
  def decimalToAnySystem(decimalNumberAndSystem: List[Int]): List[Int] =
    @tailrec
    def recHelper(decimalNumber: Int, system: Int, resultList: List[Int]): List[Int] =
      decimalNumber match
        case 0 => resultList
        case _ => recHelper(decimalNumber / system, system, (decimalNumber % system) :: resultList)

    if decimalNumberAndSystem.head == 0 then List(0)
    else if decimalNumberAndSystem.head < 0 then
      print("Podana liczba jest ujemna!\n")
      return List()
    else recHelper(decimalNumberAndSystem.head, decimalNumberAndSystem.tail.head, List())

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //Zadanie 3
  def generateTree(depth: Int): BT[Double] =
    val random = scala.util.Random
    depth match
      case 0 => Empty
      case _ => Node(random.nextDouble(), generateTree(depth - 1), generateTree(depth - 1))

  //Zadanie 4
  def treeElementMultiplicator(tree: BT[Double]): Double =
    def recHelper(toVisit: BT[Double], result: Double): Double =
      toVisit match
        case Empty => result
        case Node(value, leftSubtree, rightSubtree) => recHelper(leftSubtree, recHelper(rightSubtree, value * result))

    recHelper(tree, 1.0)

  //Zadanie 5

  def checkVisited(value: Double, list: List[Double]): List[Double] =
    list match
      case Nil => List(value)
      case h :: t =>
        if h == value then list
        else h :: checkVisited(value, t)

  def findNode(listOfUniqeNodes: List[Double], index: Int): Double =
    (listOfUniqeNodes, index) match
      case (Nil, _) => throw new Exception("Error")
      case (head :: _, 0) => head
      case (_ :: tail, _) => findNode(tail, index - 1)


  def buildNewTreeWithoutDuplicat(listOfUniqeNodes: List[Double], index: Int, treeSize: Int): BT[Double] =
    if treeSize < index then Empty
    else Node(findNode(listOfUniqeNodes, index - 1), buildNewTreeWithoutDuplicat(listOfUniqeNodes, index * 2, treeSize),
      buildNewTreeWithoutDuplicat(listOfUniqeNodes, index * 2 + 1, treeSize))

  def numberOfUniqueValue(listOfUniqeNodes: List[Double]): Int = {
    def recHelper(list: List[Double], size: Int): Int =
      list match
        case Nil => size
        case h :: t => recHelper(t, size + 1)

    recHelper(listOfUniqeNodes, 0)
  }


  def deleteDuplicatesBST(tree: BT[Double]): BT[Double] = {
    @tailrec
    def listOfNodes(queue: List[BT[Double]], list: List[Double]): List[Double] =
      queue match
        case Nil => list
        case Empty :: tail => listOfNodes(tail, list)
        case Node(value, Empty, Empty) :: tail => listOfNodes(tail, checkVisited(value, list))
        case Node(value, leftSubtree, Empty) :: tail => listOfNodes(tail ::: List(leftSubtree), checkVisited(value, list))
        case Node(value, Empty, rightSubtree) :: tail => listOfNodes(tail ::: List(rightSubtree), checkVisited(value, list))
        case Node(value, leftSubtree, rightSubtree) :: tail => listOfNodes(tail ::: List(leftSubtree, rightSubtree), checkVisited(value, list))

    val uniqeNode = listOfNodes(List(tree), List())
    buildNewTreeWithoutDuplicat(uniqeNode, 1, numberOfUniqueValue(uniqeNode))
  }

  def deleteDuplicatesDST(tree: BT[Double]): BT[Double] = {
    @tailrec
    def listOfNodes(stack: List[BT[Double]], list: List[Double]): List[Double] =
      stack match
        case Nil => list
        case Empty :: tail => listOfNodes(tail, list)
        case Node(value, Empty, Empty) :: tail => listOfNodes(tail, checkVisited(value, list))
        case Node(value, leftSubtree, Empty) :: tail => listOfNodes(leftSubtree :: tail, checkVisited(value, list))
        case Node(value, Empty, rightSubtree) :: tail => listOfNodes(rightSubtree :: tail, checkVisited(value, list))
        case Node(value, leftSubtree, rightSubtree) :: tail => listOfNodes(leftSubtree :: rightSubtree :: tail, checkVisited(value, list))

    val uniqeNode = listOfNodes(List(tree), List())
    buildNewTreeWithoutDuplicat(uniqeNode, 1, numberOfUniqueValue(uniqeNode))
  }

  def main(args: Array[String]): Unit = {

  }

  println("Zadanie 1")
  println(decimalToHex(31))
  println(decimalToHex(121))
  println(decimalToHex(-31))
  println("\nZadanie 2")
  println(decimalToAnySystem(List(31, 16)))
  println(decimalToAnySystem(List(31, 2)))
  println(decimalToAnySystem(List(-31, 2)))
  println(decimalToAnySystem(List(31, 8)))
  println("\nZadanie 3 i 4")
  val tree1 = generateTree(4)
  println("drzewo wygenerowane: " + tree1)
  println("Wynik mnożenia: " + treeElementMultiplicator(tree1))

  println("\nZadanie 5")

  val tree2 = Node(1.0, Node(2.0, Node(4.0, Empty, Empty), Node(7.0, Empty, Empty)), Node(3.0, Node(5.0, Empty, Node(6.0, Empty, Empty)), Node(4.0, Empty, Node(7.0, Empty, Empty))))
  val tree3 = Node(1.0, Node(2.0, Node(4.0, Empty, Empty), Node(7.0, Empty, Empty)), Node(3.0, Node(5.0, Empty, Node(6.0, Empty, Empty)), Node(4.0, Empty, Node(7.0, Empty, Empty))))
  /*

                    1
                  /   \
                 /     \
                /       \
               /         \
              2           3
            /   \       /   \
           4     7     5     4
         /  \   / \   / \   / \
        E    E E   E E   6 E   7
                        / \   / \
                       E   E E   E

*/
  println("Drzewo przed usuwaniem: " + tree2)
  println("BST:")
  println(deleteDuplicatesBST(tree2))
  println("DST:")
  println(deleteDuplicatesDST(tree3))
}
