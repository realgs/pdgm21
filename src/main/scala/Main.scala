import scala.annotation.tailrec
import scala.collection.{GenMap, mutable}
import scala.util.Random

object Main {
    sealed trait BT[A]
    case class Empty[A]() extends BT[A]
    case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]



    def toHex(number: Int): List[Int]=
      @tailrec
      def toHexTailrec(number: Int, list: List[Int]): List[Int]=
        if number != 0 then
          toHexTailrec(number/16, List(number%16) ::: list)
        else list
      toHexTailrec(number, Nil)

  //zdecydowałem się na dwa argumenty zamiast listy dla przejrzystości użycia tej funkcji
    def changeBase(number: Int, base: Int): List[Int]=
      @tailrec
      def changeBaseTailrec(number: Int, list: List[Int]): List[Int]=
        if number != 0 then
          changeBaseTailrec(number/base, List(number%base) ::: list)
        else list
      changeBaseTailrec(number, Nil)

    def createNLevelTree(n: Int):Node[Double] =
      if n==1 then Node(Random.nextDouble(),Empty[Double](),Empty[Double]())
      else Node(Random.nextDouble(),createNLevelTree(n-1),createNLevelTree(n-1))

    def treeProduct(tree: BT[Double]): Double =
      tree match
        case Empty() => 1
        case Node(elem, left, right) => elem*treeProduct(left)*treeProduct(right)

    def contains[A](list: List[A], elem: A): Boolean =
      list match
        case Nil => false
        case h::t => if h == elem then true else contains(t, elem)

    def nodeContains[A](list: List[BT[A]], elem: A): Boolean =
      list match
        case Nil => false
        case h::t =>
          h match
            case Empty() => false
            case Node(value, _, _) => if value == elem then true else nodeContains(t, elem)

    def depthFirstSearch[A](node: BT[A]): List[BT[A]]=
      node match
        case Empty() => Nil
        case Node(elem, left, right) => depthFirstSearch(left) ::: List(node) ::: depthFirstSearch(right) //inorder traversal

    def dfsWithoutDuplicates[A](node: BT[A]): List[A]=
      var finalList: List[A] = List()
      def helperDFS(node: BT[A]): Unit=
        node match
          case Empty() =>
          case Node(elem, left, right) =>
            helperDFS(left)
            if !contains(finalList, elem) then finalList = finalList:::List(elem)
            helperDFS(right)
      helperDFS(node)
      finalList

    def breadthFirstSearch[A](queue: List[BT[A]]): List[BT[A]] =
      queue match
        case h::t =>
          h match
            case Node(elem, left, right) => h :: breadthFirstSearch(nodeEnqueue(nodeEnqueue(t, left), right))
            case Empty() => Nil
        case Nil => Nil

    def bfsWithoutDuplicates[A](node: BT[A]): List[A]=
      var result: List[A] = List()
      def bfsHelper(queue: List[BT[A]]): Unit=
        queue match
          case h::t =>
            h match
              case Node(elem, left, right) =>
                if !contains(result, elem) then result = result ::: List(elem)
                bfsHelper(nodeEnqueue(nodeEnqueue(t,left), right))
              case Empty() =>
          case Nil =>
      bfsHelper(List(node))
      result

    def printTree[A](node: Node[A]): Unit=
      node match
        case Node(elem, left: Node[A], right: Node[A])=> println(elem); printTree(left); printTree(right);
        case Node(elem, left: Node[A], Empty())=> println(elem); printTree(left);
        case Node(elem, Empty(), right: Node[A])=> println(elem); printTree(right);
        case Node(elem, _, _) => println(elem)

    def nodeEnqueue[A](queue: List[BT[A]], elem: BT[A]): List[BT[A]] =
      queue match
        case h::t => h :: nodeEnqueue(t, elem)
        case Nil => List(elem)

    def nodeDequeue[A](queue: List[BT[A]]): BT[A] =
      queue match
        case h::t => h
        case Nil => throw new Exception("Empty queue exception")

    def btListToValues[A](list: List[BT[A]]): List[A] =
      list match
        case Nil => Nil
        case h::t =>
          h match
            case Empty() => Nil
            case Node(elem, left, right) => elem:: btListToValues(t)

    def getElemAtPos[A](list: List[A], pos: Int): A =
      (pos, list) match
        case (1,  h::t) => h
        case (_, h::t) => getElemAtPos(t, pos-1)
        case (_, Nil) => throw  new Exception("Out of bounds exception")

    def length[A](list: List[A]): Int=
      list match
        case Nil => 0
        case h::t => 1+length(t)

    def createTreeFromList[A](list: List[A]): Node[A] =
      def createTree(pos: Int): Node[A] =
        Node(getElemAtPos(list, pos),
          if pos*2 > length(list) then Empty() else createTree(pos*2),
          if pos*2+1 > length(list) then Empty() else createTree(pos*2+1))
      createTree(1)

    def main(args: Array[String]): Unit =
      //zadanie 1
      println("EX 1")
      println(toHex(31))
      println(toHex(124))
      println(toHex(21563))

      //zadanie 2
      println("\nEX 2")
      println(changeBase(128, 2))

      //zadanie 3
      println("\nEX 3")
      val tree = createNLevelTree(4)
      println(tree)

      //zadanie 4
      println("\nEX 4")
      println(treeProduct(tree))

      //zadanie 5
      println("\nEX 5")
      val treeWithoutDuplicates = Node(9, Node(4, Node(2, Empty(), Empty()), Node(1, Empty(), Empty())), Node(8, Node(6, Empty(), Empty()), Node(5, Empty(), Empty())))
      //printTree(treeWithoutDuplicates)

      val treeWithDuplicates = Node(9, Node(4, Node(9, Empty(), Empty()), Node(1, Empty(), Empty())), Node(3, Node(1, Empty(), Empty()), Node(5, Empty(), Empty())))
      printTree(treeWithDuplicates)

      /*
      val dfs = depthFirstSearch(tree)
      val bfs = breadthFirstSearch(List(tree))
      val dfsWithoutDups = dfsWithoutDuplicates(treeWithDuplicates)
      val bfsWithoutDups = bfsWithoutDuplicates(treeWithDuplicates)
      println("\nDFS\n")
      println(dfsWithoutDups)
      println("\nBFS\n")
      println(bfsWithoutDups)*/

      val newTreeDFS = createTreeFromList(dfsWithoutDuplicates(treeWithDuplicates))
      println("\nDFS")
      print(newTreeDFS)
      val newTreeBFS = createTreeFromList(bfsWithoutDuplicates(treeWithDuplicates))
      println("\nBFS")
      print(newTreeBFS)
  }