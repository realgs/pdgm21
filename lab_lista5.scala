object lab_lista5 {

  import scala.util.Random

  //zadanie 1
  def toHexadecimal(n: Int): List[Int] =
  {
    def toHexadecimalHelper(n: Int, result: List[Int]): List[Int] =
    {
      if n == 0 then result
      else toHexadecimalHelper(n / 16, n % 16 :: result)
    }
    if n > 0 then toHexadecimalHelper(n, Nil)
    else if n == 0 then List(0)
    else (-1)::toHexadecimalHelper(-n, Nil)
  }

  //zadanie 2
  def toAnySystem(n: Int, system: Int): List[Int] =
  {
    def toAnySystemHelper(n: Int, system: Int, result: List[Int]): List[Int] =
    {
      if n == 0 then result
      else toAnySystemHelper(n / system, system, n % system :: result)
    }
    if system < 2 then Nil
    else
      if n > 0 then toAnySystemHelper(n, system, Nil)
      else if n == 0 then List(0)
      else (-1) :: toAnySystemHelper(-n, system, Nil)
  }

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = new Random()

  //zadanie 3
  def generateTree(tree_depth: Int): BT[Double] =
  {
    if tree_depth <= 0 then Empty
    else Node(random.nextDouble(), generateTree(tree_depth-1), generateTree(tree_depth-1))
  }

  //zadanie 4
  def productOfTreeElements(tree: BT[Double]): Double =
  {
    def productHelper(tree: BT[Double], result: Double): Double =
      {
        tree match {
          case Empty => result
          case Node(value, left_subtree, right_subtree) => productHelper(left_subtree, productHelper(right_subtree, result*value))
        }
      }
    if tree == Empty then 0
    else productHelper(tree, 1)
  }

  def breadthBT[A](tree: BT[A]): List[A] =
    def breadthBTHelper[A](queue: List[BT[A]]): List[A] =
      queue match {
        case Nil => Nil
        case Empty :: tail => breadthBTHelper(tail)
        case Node(value, l_subtree, r_subtree) :: tail => value :: breadthBTHelper(tail ::: List(l_subtree, r_subtree))
      }
    breadthBTHelper(List(tree))

  def depthBT[A](tree: BT[A]): List[A] =
    def depthBTHelper[A](toVisit: BT[A], labels: List[A]): List[A] =
      {
        (toVisit, labels) match {
          case (Empty, labels) => labels
          case (Node(value, l_subtree, r_subtree), labels) => value::depthBTHelper(l_subtree, depthBTHelper(r_subtree, labels))
        }
      }
    depthBTHelper(tree, Nil)

  def generateTreeInt(tree_depth: Int, min: Int, max: Int): BT[Int] =
  {
    if tree_depth <= 0 then Empty
    else Node(random.between(min, max+1), generateTreeInt(tree_depth-1, min, max), generateTreeInt(tree_depth-1, min, max))
  }

  //zadanie 5
  def removeDuplicates[A](list: List[A]): List[A] =
  {
    def helper[A](list: List[A], result: List[A]): List[A] =
      {
        list match {
          case Nil => result.reverse
          case head::tail => if result.contains(head) then helper(tail, result) else helper(tail, head::result)
        }
      }
    helper(list, Nil)
  }

  def getElement[A](list: List[A], index: Int): A =
  {
    (list, index) match {
      case (head::tail, 0) => head
      case (head::tail, _) => getElement(tail, index-1)
      case (Nil, _) => throw new Exception("Index outside the list")
    }
  }

  def makeTreeFromList[A](list: List[A]): BT[A] =
  {
    val list_size = list.size
    def makeTreeFromListHelper[A](list: List[A], index: Int, size: Int): BT[A] =
      {
        if size > index then Node(getElement(list, index), makeTreeFromListHelper(list, 2*index+1, size), makeTreeFromListHelper(list, 2*index+2, size))
        else Empty
      }
    if list == Nil then Empty
    else makeTreeFromListHelper(list, 0, list_size)
  }

  def removeDuplicatesDFS[A](tree: BT[A]): BT[A] =
  {
    val list_without_duplicates = removeDuplicates(depthBT(tree))
    if list_without_duplicates != depthBT(tree) then makeTreeFromList(list_without_duplicates) else tree
  }

  def removeDuplicatesBFS[A](tree: BT[A]): BT[A] =
  {
    val list_without_duplicates = removeDuplicates(breadthBT(tree))
    if list_without_duplicates != breadthBT(tree) then makeTreeFromList(list_without_duplicates) else tree
  }

    def main(args: Array[String]): Unit = {
      println(toHexadecimal(50))
      println(toHexadecimal(-400))
      println(toAnySystem(-2107, 5))
      println(toAnySystem(2137, 2))
      println()

      val tree_1 = generateTree(2)
      println(tree_1)
      println(productOfTreeElements(tree_1))
      println()

      val tree_2 = generateTreeInt(3, 1, 10)
      println(tree_2)
      println(removeDuplicatesBFS(tree_2))
      println(removeDuplicatesDFS(tree_2))
  }

}
