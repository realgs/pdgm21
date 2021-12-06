import scala.util.Random

object lab5 {
  //Zadanie 1
  def decimalToHex(decNumber: Int): List[Int] =
    def decimalToHexI(decNumber: Int, hexRepresentation: List[Int]) : List[Int] =
      if decNumber == 0 then hexRepresentation
      else decimalToHexI(decNumber / 16, decNumber % 16 :: hexRepresentation)
    if decNumber > 0 then decimalToHexI(decNumber, Nil)
    else if decNumber == 0 then List(0)
    else throw new Exception("Wrong input")

  //Zadanie 2
  def decimalToAny(decNumber : Int, base: Int): List[Int] =
    def decimalToAnyI(decNumber: Int, otherRepresentation: List[Int]) : List[Int] =
      if decNumber == 0 then otherRepresentation
      else decimalToAnyI(decNumber / base, decNumber % base::otherRepresentation)
    if decNumber > 0 && base > 1 then decimalToAnyI(decNumber, Nil)
    else if decNumber == 0 && base > 1 then List(0)
    else throw new Exception("Wrong input")


  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val randomGen = Random()

  //Zadanie 3
  def genTree(N : Int): BT[Double] =
    if N <= 0 then Empty
    else Node(randomGen.nextDouble(), genTree(N - 1), genTree(N - 1))

  def genTreeInt(N : Int): BT[Int] =
    if N <= 0 then Empty
    else Node(randomGen.nextInt(5), genTreeInt(N - 1), genTreeInt(N - 1))

  //Zadanie 4
  def prodOfTree(tree: BT[Double]): Double =
    tree match
      case Empty => 1
      case Node(elem, left, right) => elem * prodOfTree(left) * prodOfTree(right)

  //Zadanie 5
  def findNthElem[A](list: List[A], N: Int): A =
    (list, N) match
      case (Nil, _) => throw new Exception("Element doesn't exist")
      case (elem :: _, 0) => elem
      case (_ :: t, index) => findNthElem(t, N - 1)

  def treeFromList[A](elems: List[A], index: Int, size: Int): BT[A] =
    if index > size then Empty
    else Node(findNthElem(elems, index - 1), treeFromList(elems, index * 2, size), treeFromList(elems, index * 2 + 1, size))

  def removeDuplicatesDFS[A](tree: BT[A]): BT[A] =
    def createUniqueElemsListDFS(treeStack: List[BT[A]], uniqueElems: List[A]): List[A] =
      treeStack match
        case Nil => uniqueElems.reverse
        case Empty :: t => createUniqueElemsListDFS(t, uniqueElems)
        case Node(elem, left, right) :: t =>
          if uniqueElems.contains(elem) then createUniqueElemsListDFS(left :: right :: t, uniqueElems)
          else createUniqueElemsListDFS(left :: right :: t, elem :: uniqueElems)
    val uniqueElemsList = createUniqueElemsListDFS(List(tree), Nil)
    treeFromList(uniqueElemsList, 1, uniqueElemsList.length)

  def removeDuplicatesBFS[A](tree: BT[A]): BT[A] =
    def createUniqueElemsListBFS(treeQueue: List[BT[A]], uniqueElems: List[A]): List[A] =
      treeQueue match
        case Nil => uniqueElems.reverse
        case Empty :: t => createUniqueElemsListBFS(t, uniqueElems)
        case Node(elem, left, right) :: t =>
          if uniqueElems.contains(elem) then createUniqueElemsListBFS(t ::: List(left, right), uniqueElems)
          else createUniqueElemsListBFS(t ::: List(left, right), elem :: uniqueElems)
    val uniqueElemsList = createUniqueElemsListBFS(List(tree), Nil)
    treeFromList(uniqueElemsList, 1, uniqueElemsList.length)

  def main(args: Array[String]): Unit = {
    println(decimalToHex(31))
    println(decimalToAny(31, 16))
    println(decimalToAny(10, 2))
    println(decimalToAny(0, 2))

    println(genTree(0))
    println(genTree(1))
    println(genTree(2))
    println(genTreeInt(3))

    val tree = genTree(2)
    println(tree)
    println(prodOfTree(tree))

    val tree2 = genTreeInt(3)
    println(tree2)
    println(removeDuplicatesBFS(tree2))
    println(removeDuplicatesDFS(tree2))
  }
}
