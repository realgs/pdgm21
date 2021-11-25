import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object list_5 {

  sealed trait BT
  case object Empty extends BT
  case class Node(val x: Int, val left: BT, val right: BT) extends BT

  //converts decimal number 'n' into List of hex digits
  //when 'n' < 0 then List.head = -1
  def convertTo_hexadecimal(n: Int): List[Int] = {
    @tailrec
    def cT_h_inner(x: Int, hexadecimal: List[Int]): List[Int] =
      if x == 0 then hexadecimal
      else cT_h_inner(x / 16, (x % 16) :: hexadecimal)

    if n < 0 then -1 :: cT_h_inner(-1 * n, Nil)
    else cT_h_inner(n, Nil)
  }

  //converts decimal number 'n' into List of given base digits
  //when 'n' < 0 then List.head = -1
  def convertTo_any(n: Int, base: Int): List[Int] = {
    @tailrec
    def cT_a_inner(x: Int, hexadecimal: List[Int]): List[Int] =
      if x == 0 then hexadecimal
      else cT_a_inner(x / base, (x % base) :: hexadecimal)

    if n < 0 then -1 :: cT_a_inner(-1 * n, Nil)
    else cT_a_inner(n, Nil)
  }

  //create perfect binarytree
  //nodes have random values between 1 and 10 inclusive (2147483647 if maxvalue is set to Int.MaxValue - 1)
  def createTree(N: Int): BT = {
    def cT_inner(depth: Int, maxvalue: Int): BT =
      if depth == N then Node(Random.nextInt(maxvalue) + 1, Empty, Empty)
      else
        Node(Random.nextInt(maxvalue) + 1, cT_inner(depth + 1, maxvalue), cT_inner(depth + 1, maxvalue))

    cT_inner(0, 10) //Int.MaxValue - 1)
  }

  //multiplies values of all nodes (using dfs)
  //for depth = 14 it's almost instant
  //time complexity: 2^(DEPTH+1)-1 (O(N))
  //memory: BigInt eats memory, there is never more than DEPTH + 1 elements in a list
  def multiplyNodes(tree: BT): BigInt = {
    @tailrec
    def mN_inner(acc: BigInt, LIFO: List[BT]): BigInt =
      LIFO match
        case Nil => acc
        case Empty::t => mN_inner(acc, t)
        case Node(x, Empty, Empty)::t => mN_inner(acc * x, t)
        case Node(x, left, Empty)::t => mN_inner(acc * x, left::t)
        case Node(x, Empty, right)::t => mN_inner(acc * x, right::t)
        case Node(x, left, right)::t => mN_inner(acc * x, left::right::t)

    mN_inner(1, List(tree))
  }

  //remove recurring nodes (avalanche like -ish)
  //time: ~2^(3*DEPTH) (O(N^3))
  //memory: ~2^(2*DEPTH) (O(N^2))
  def removeRecurring_dfs(bt: BT, found: mutable.SortedSet[Int]): BT = {
    //finds recurrig nodes uising dfs
    //time: 2^(DEPTH+1) - 1 to traverse; for each recurring node c(removeNode)*c(findPath)
    //memory: 2^(DEPTH+1) - 1 Nodes; worst case same amount of values in 'found'
    def dfs_helper(tree: BT): BT = {
      tree match
        case Empty => Empty
        case Node(x, _, _) if found.contains(x) => dfs_helper(removeNode(tree, findPath(tree)))
        case Node(x, Empty, Empty) =>
          found += x
          tree
        case Node(x, left, Empty) =>
          found += x
          Node(x, dfs_helper(left), Empty)
        case Node(x, Empty, right) =>
          found += x
          Node(x, Empty, dfs_helper(right))
        case Node(x, left, right) =>
          found += x
          Node(x, dfs_helper(left), dfs_helper(right))
    }

    //replaces root node with one of children along the longest internal path
    //time: path.lenght
    //memory: path.lenght + p.l -1 + ... + 1; path.lenght nodes
    def removeNode(tree: BT, path: List[Int]): BT = {
      (path, tree) match
        case (_, Empty) => Empty
        case (h :: t, Node(_, left, right)) if h == -1 =>
          left match
            case Empty => Empty
            case Node(x0, _, _) => Node(x0, removeNode(left, t), right)
        case (h :: t, Node(_, left, right)) =>
          right match
            case Empty => Empty
            case Node(x0, _, _) => Node(x0, left, removeNode(right, t))
        case (Nil, _) => Empty
    }

    //finds longest internal path, returns that path as a list
    //value -1 is left node
    //value 1 is right node
    //time: 2^(DEPTH + 1) - 1
    //memory: (2^d possible paths of leght = d) for d = 1, 2,..., DEPTH; 2^(DEPTH + 1) - 1 Nodes
    def findPath(tree: BT): List[Int] = {
      def fP_inner(t: BT, depth: Int, currentPath: List[Int]): (Int, List[Int]) =
        t match
          case Empty => (depth, currentPath)
          case Node(_, left, right) =>
            val d1 = fP_inner(left, depth + 1, -1 :: currentPath)
            val d2 = fP_inner(right, depth + 1, 1 :: currentPath)
            if d1._1 >= d2._1 then d1
            else d2

      fP_inner(tree, 0, Nil)._2.tail.reverse
    }
    dfs_helper(bt)
  }

  //replaces reccuring nodes value with '0'
  //time ~2^DEPTH (O(N))
  //memory: ~2^DEPTH (O(N))
  def removeRecurring_bfs(bt: BT, found: mutable.SortedSet[Int]): BT = {

    //builds tree from a list of values (list is created by bfs)
    //time 2^(DEPTH+1)-1
    //memory: 2^DEPTH for new ::tmp (each time /2 less)
    @tailrec
    def buildTree(values: List[Int], built: List[BT], tmp: List[BT]): BT = {
      (values, built) match
        case (_, Nil) => buildTree(values, tmp.reverse, Nil)
        case (Nil, _) => built.head
        case (h1::t1, x0::x1::t2) =>
          buildTree(t1, t2, Node(h1, x1, x0)::tmp)
    }

    //replaces recccuring values with '0'
    ////time 2^(DEPTH+1)-1
    //memory: 2^DEPTH for new ::acc
    @tailrec
    def replaceRecurring(values: List[Int], acc: List[Int]): List[Int] = {
      values match
        case Nil => acc
        case h::t if found.contains(h) => replaceRecurring(t, 0::acc)
        case h::t =>
          found += h
          replaceRecurring(t, h::acc)
    }

    //returns list of values in a tree
    ////time 2^(DEPTH+1)-1
    //memory: 2^DEPTH for new ::acc
    @tailrec
    def bfs_prepare(queue: mutable.Queue[BT], acc: List[Int]): List[Int] = {
      if queue.isEmpty then acc
      else
        queue.dequeue() match
          case Empty => bfs_prepare(queue, acc)
          case Node(x, left, right) => bfs_prepare(queue.enqueue(left, right), x::acc)
    }

    //returns list of 'Empty' trees of size N (I needed this to simplyfi buildTree function)
    ////time 2^DEPTH+1
    //memory: 2^(DEPTH + 1) for new ::acc
    @tailrec
    def makeEmptyList(N: Int, acc: List[BT]): List[BT] = {
      if N == 0 then acc
      else makeEmptyList(N-1, Empty::acc)
    }

    var list = replaceRecurring(
      bfs_prepare(new mutable.Queue[BT]().enqueue(bt), Nil).reverse,
      Nil
    )

    buildTree(list, makeEmptyList(
      list.length+1, Nil
    ), Nil)
  }

  //returns list of nodes in a tree
  //uses dfs to traverse the graph
  def printTree(tree: BT): List[Int] = {
    @tailrec
    def pT_inner(queue: mutable.Queue[BT], acc:List[Int]): List[Int] =
      if queue.isEmpty then acc
      else
        queue.dequeue() match
          case Empty => pT_inner(queue, -1::acc)
          case Node(x, left, right) => pT_inner(queue.enqueue(left, right), x::acc)
    pT_inner(new mutable.Queue[BT]().enqueue(tree), Nil).reverse
  }

  def main(args: Array[String]):Unit = {
    println("31 in hex: " + convertTo_hexadecimal(31))
    println("-31 in hex: " + convertTo_hexadecimal(-31))
    println("31 in base 7: " + convertTo_any(31,7))
    println("-31 in base 7: " + convertTo_any(-31,7))

    var tree = createTree(3)
    println("Product: " + multiplyNodes(tree))
    println("Original tree: " + printTree(tree))
    var test1 = removeRecurring_dfs(tree,mutable.SortedSet[Int]())
    var test2 = removeRecurring_bfs(tree,mutable.SortedSet[Int]())
    println("tree after dfs: " + printTree(test1))
    println("tree after bfs: " + printTree(test2))

  }
}
