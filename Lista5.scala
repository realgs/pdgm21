import scala.annotation.tailrec

object Lista5 {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val r = scala.util.Random

  //Zad 1
  def decimalToHexadecimal(number: Int): List[Int] = {
    @tailrec
    def decimalToHexadecimalHelper(number: Int, result: List[Int]): List[Int] =
      if number == 0 then result
      else decimalToHexadecimalHelper(number / 16, number % 16 :: result)

    decimalToHexadecimalHelper(number, Nil)
  }

  //Zad 2
  def decimalToAny(number: Int, system: Int): List[Int] = {
    if system <= 1 then throw new Exception("Wrong system number!")

    @tailrec
    def decimalToHexadecimalHelper(number: Int, result: List[Int]): List[Int] =
      if number == 0 then result
      else decimalToHexadecimalHelper(number / system, number % system :: result)

    decimalToHexadecimalHelper(number, Nil)
  }

  //Zad 3
  def createTree[A](N: Int): BT[Double] = {
    if (N == 0) then Node(r.nextDouble, Empty, Empty)
    else Node(r.nextDouble, createTree(N - 1), createTree(N - 1))
  }

  //Zad 4
  def countTree[A](tree: BT[Double]) = {
    @tailrec
    def countTreeHelper[A](queue: List[BT[Double]], result: Double): Double = queue match
      case Nil => result
      case Empty :: tail => countTreeHelper(tail, result)
      case Node(value, leftSubTree, rightSubTree) :: tail => countTreeHelper(tail ::: List(leftSubTree, rightSubTree), value * result)

    countTreeHelper(List(tree), 1)
  }

  //Zad 5
  def createTreeFromList[A](list: List[A]): BT[A] = {
    list match
      case Nil => Empty
      case h :: t =>
        val array = t.splitAt(t.length / 2)
        Node(h, createTreeFromList(array._1), createTreeFromList(array._2))
  }

  def breadthBT[A](tree: BT[A]): List[A] = {
    def breadthBTHelper[A](queue: List[BT[A]]): List[A] = queue match
      case Nil => Nil
      case Empty :: tail => breadthBTHelper(tail)
      case Node(value, leftSubTree, rightSubTree) :: tail => value :: breadthBTHelper(tail ::: List(leftSubTree, rightSubTree))

    breadthBTHelper(List(tree))
  }


  @tailrec
  def contains[A](value: A, list: List[A]): Boolean = {
    list match
      case Nil => false
      case h :: t =>
        if value == list.head then true
        else contains(value, list.tail)
  }

  def deleteDuplicatesBread[A](tree: BT[A]): BT[A] = {
    def deleteDuplicatesBreadHelper[A](queue: List[BT[A]], visited: List[A]): List[A] = queue match
      case Nil => Nil
      case Empty :: tail => deleteDuplicatesBreadHelper(tail, visited)
      case Node(value, leftSubTree, rightSubTree) :: tail =>
        if (contains(value, visited)) then deleteDuplicatesBreadHelper(tail ::: List(leftSubTree, rightSubTree), visited)
        else value :: deleteDuplicatesBreadHelper(tail ::: List(leftSubTree, rightSubTree), value :: visited)

    createTreeFromList(deleteDuplicatesBreadHelper(List(tree), Nil))
  }


  def deleteDuplicatesDepth[A](tree: BT[A]): BT[A] = {
    def deleteDuplicatesDepthHelper[A](stack: List[BT[A]], visited: List[A]): List[A] =
      stack match
        case Nil => Nil
        case Empty :: tail => deleteDuplicatesDepthHelper(tail, visited)
        case Node(value, left, right) :: tail =>
          if (contains(value, visited)) then deleteDuplicatesDepthHelper(left :: right :: tail, visited)
          else value :: deleteDuplicatesDepthHelper(left :: right :: tail, value :: visited)

    createTreeFromList(deleteDuplicatesDepthHelper(List(tree), Nil))
  }


  def main(args: Array[String]): Unit = {
    val tree = createTree(3)
    println(breadthBT(tree))
    println(countTree(tree))
    println(contains(4, List(1, 2, 3)))
    println(createTreeFromList(List(1, 2, 3, 3, 4)))
    println(breadthBT(createTreeFromList(List(1, 2, 3, 3, 4))))
    println(deleteDuplicatesBread(createTreeFromList(List(1, 2, 3, 3, 4))))
    println(breadthBT(deleteDuplicatesBread(createTreeFromList(List(1, 2, 3, 3, 4)))))
    println(breadthBT(deleteDuplicatesDepth(createTreeFromList(List(1, 2, 3, 3, 4)))))
    println(decimalToHexadecimal(123))
    println(decimalToAny(123, 2))
    println(decimalToAny(123, 0))
  }
}

