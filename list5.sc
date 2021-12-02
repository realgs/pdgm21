import scala.annotation.tailrec

object main {
  def reverse[A](xs: List[A]): List[A] =
    def reverseRec[A](xs: List[A], x: List[A]): List[A] =
      if xs == List() then x
      else reverseRec(xs.tail, xs.head :: x)

    reverseRec(xs, List())

  //Zadanie 1
  def decToHex(value: Int): List[Int] =
    if value < 0 then Nil
    else
      @tailrec
      def decToHexRec(value: Int, result: List[Int]): List[Int] =
        value match
          case 0 => result
          case _ => decToHexRec(value / 16, value % 16 :: result)

      decToHexRec(value / 16, List(value % 16))

  //Zadadnie 2
  def decToXSystem(value: Int, system: Int): List[Int] =
    if value < 0 then Nil
    else
      @tailrec
      def decToXSystemRec(value: Int, result: List[Int]): List[Int] =
        value match
          case 0 => result
          case _ => decToXSystemRec(value / system, value % system :: result)

      decToXSystemRec(value / system, List(value % system))

  //Drzewo
  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //Zadanie 3
  val random = scala.util.Random

  def newTree(depth: Int): BT[Double] =
    if depth > 0 then
      Node(random.nextDouble(), newTree(depth - 1), newTree(depth - 1))
    else Empty


  //Zadanie 4
  def treeMultiply(tree: BT[Double]): Double =
    tree match
      case Node(elem, left, right) => elem * treeMultiply(left) * treeMultiply(right)
      case Empty => 1

  //Zadanie 5
  def splitList[A](list: List[A]): (List[A], List[A]) =
    def splitListRec(list: List[A], result: (List[A], List[A])): (List[A], List[A]) =
      list match
        case t1 :: t2 :: tail => splitListRec(tail, (t1 :: result._1, t2 :: result._2))
        case t1 :: tail => splitListRec(tail, (t1 :: result._1, result._2))
        case _ => result

    splitListRec(list, (Nil, Nil))

  def createTreeFromList(list: List[Double]): BT[Double] =
    def createTreeFromListRec(list: List[Double]): BT[Double] =
      list match
        case t1 :: tail =>
          var halfs = splitList(tail)
          Node(t1, createTreeFromListRec(halfs._1), createTreeFromListRec(halfs._2))
        case _ => Empty

    createTreeFromListRec(list)


  def duplicateRemoverDFS(tree: BT[Double]) =
    def DFS(list: List[BT[Double]], nodesValues: List[Double]): List[Double] =
      list match
        case Empty :: tail => DFS(tail, nodesValues)
        case Node(elem, left, right) :: tail =>
          if nodesValues.contains(elem) then DFS(left :: right :: tail, nodesValues)
          else DFS(left :: right :: tail, elem :: nodesValues)
        case Nil => reverse(nodesValues)

    createTreeFromList(DFS(tree :: Nil, Nil))

  def duplicateRemoverBFS[A](tree: BT[Double]) =
    def BFS(queue: List[BT[Double]], nodesValues: List[Double]): List[Double] =
      queue match
        case Empty :: tail => BFS(tail, nodesValues)
        case Node(elem, left, right) :: tail =>
          if nodesValues.contains(elem) then BFS(tail ::: left :: right :: Nil, nodesValues)
          else BFS(tail ::: left :: right :: Nil, elem :: nodesValues)
        case Nil => reverse(nodesValues)

    createTreeFromList(BFS(tree :: Nil, Nil))

  def main(args: Array[String]): Unit = {
    println(decToHex(255))
    println(decToHex(256))
    println(decToXSystem(255, 10))
    println(decToXSystem(256, 10))
    println(newTree(2))
    println(treeMultiply(newTree(2)))
    println(duplicateRemoverDFS(Node(0.1, Node(0.2, Node(0.2, Empty, Empty), Node(0.2, Empty, Empty)), Node(0.8, Empty, Empty))))
    println(duplicateRemoverBFS(Node(0.1, Node(0.5, Node(0.5, Empty, Empty), Empty), Node(0.8, Node(0.5, Empty, Empty), Empty))))
  }
}
