import java.util.InputMismatchException
import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  // checks if list contains element
  @tailrec
  def myContains[A](elem: A, list: List[A]): Boolean = {
    list match
      case Nil => false
      case x :: xs => if x == elem then true else myContains(elem, xs)
  }

  // returns length of a list
  def myLength[A](list: List[A]): Int = {
    list match
      case x :: xs => 1 + myLength(xs)
      case _ => 0
  }

  // ---- ---- task 1 ---- ----
  // worse time complexity than decToHex2
  def decToHex1(decNum: Int): List[Int] = {
    // time complexity:  log16(n) * [ log16(n) * (log16(n) - 1) / 2 ]
    // memory complexity: log16(n)
    // where n is decimal number from input

    if decNum < 0 then decToHex1(-decNum)
    else if decNum < 16 then List(decNum)
    else decToHex1(decNum / 16) ::: List(decNum % 16)

  }

  // better time complexity, comparable memory complexity to decToHex1
  def decToHex2(decNum: Int): List[Int] = {
    // time complexity: log16(n)
    // memory complexity: 1 + log16(n)
    // where n is decimal number from input
    @tailrec
    def decToHecIter(d: Int, result: List[Int]): List[Int] = {
      if d < 16 then d :: result
      else decToHecIter(d / 16, (d % 16) :: result)
    }

    decToHecIter(Math.abs(decNum), List())
  }

  // ---- ---- task 2 ---- ----
  // worse time complexity than decToAnything2
  def decToAnything1(decNum: Int, base: Int): List[Int] = {
    // time complexity:  logx(n) * [ logx(n) * (logx(n) - 1) / 2 ]
    // memory complexity: 2 * logx(n)
    // where x is base of new numerical system and n is decimal number from input

    if base <= 0 then throw new InputMismatchException()

    if decNum < 0 then decToAnything1(-decNum, base)
    else if decNum < base then List(decNum)
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

  // ---- ---- task 3 ---- ----
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

  // ---- ---- task 4 ---- ----
  // returns product of all elements in a binary tree
  def product(tree: BT[Double]): Double = {
    // time complexity: n
    // memory complexity: n * current_tree_size = n * 0.5 * n * (n-1)
    //    explanation: n calls of product(), each containing some subtree in argument
    // where n = [2 ^ (N + 1) - 1] is number of elements in the tree from input and N is depth of said tree

    tree match
      case Empty => 1
      case Node(x, left, right) => x * product(left) * product(right)
  }

  // ---- ---- task 5 ---- ----


  /*
/* Given a binary search tree and a key,
this function deletes a given key and
returns root of modified tree */
static node deleteNode(node root, int key)
{
    // base case
    if (root == null) return root;

    // If the key to be deleted is smaller than the
    // root's key, then it lies in left subtree
    if (key < root.key)
        root.left = deleteNode(root.left, key);

    // If the key to be deleted is greater than
    // the root's key, then it lies in right subtree
    else if (key > root.key)
        root.right = deleteNode(root.right, key);

    // if key is same as root's key
    else
    {
        // If key is present more than once,
        // simply decrement count and return
        if (root.count > 1)
        {
            (root.count)--;
            return root;
        }

        // ElSE, delete the node

        // node with only one child or no child
        if (root.left == null)
        {
            node temp = root.right;
            root=null;
            return temp;
        }
        else if (root.right == null)
        {
            node temp = root.left;
            root = null;
            return temp;
        }

        // node with two children: Get the inorder
        // successor (smallest in the right subtree)
        node temp = minValueNode(root.right);

        // Copy the inorder successor's
        // content to this node
        root.key = temp.key;
        root.count = temp.count;

        // Delete the inorder successor
        root.right = deleteNode(root.right,
                                temp.key);
    }
    return root;
}
*/

  // creates fairly balanced binary tree from values passed in a list
  def createTreeFromList[A](elements: List[A]): BT[A] = {
    elements match
      case Nil => Empty
      case x :: xs =>
        val size = myLength(elements)
        val (firstHalf, secondHalf) = xs.splitAt(size / 2)
        Node(x, createTreeFromList(firstHalf), createTreeFromList(secondHalf))
  }

  // traverses the tree using DFS and removes duplicates
  def removeDuplicatesDFS[A](tree: BT[A]): BT[A] = {
    // 'cuts out' duplicated elements and stores visited elements in a list
    def iter[A](tree: BT[A], elements: List[A]): List[A] =
      tree match
        case Empty => elements // finished traversing the (sub)tree
        case Node(x, left, right) =>
          // traverse entire left subtree first, then the right subtree
          if myContains(x, elements) then iter(right, iter(left, elements))
          else iter(right, iter(left, x :: elements))

    // the order of list returned by iter is reversed to somewhat preserve original order of elements
    createTreeFromList(myReverse(iter(tree, List())))
  }

  // traverses the tree using BFS and removes duplicates
  def removeDuplicatesBFS[A](tree: BT[A]): BT[A] = {
    // 'cuts out' duplicated elements and stores visited elements in a list
    @tailrec
    def iter(treeQueue: List[BT[A]], elements: List[A]): List[A] =
      treeQueue match
        case Nil => elements
        case Empty :: t=> iter(t, elements) // finished traversing the (sub)tree
        case Node(x, left, right) :: t =>
          if myContains(x, elements) then
            iter(t:::List(left, right), elements)
          else
            iter(t:::List(left, right), x :: elements)

    // the order of list returned by iter is reversed to somewhat preserve original order of elements
    createTreeFromList(myReverse(iter(List(tree), List())))
  }


  def main(args: Array[String]): Unit = {

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
        case Node(x: Double, Empty, Empty) => if x < 1 && x > 0 then true else false
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

    println(removeDuplicatesDFS(Empty) == Empty)

    println(removeDuplicatesDFS(Node(.4, Empty, Empty)) == Node(.4, Empty, Empty))

    println(removeDuplicatesDFS(Node(.3, Node(.4, Empty, Empty), Node(.4, Empty, Empty)))
      == Node(.3, Node(.4, Empty, Empty), Empty))

    println(removeDuplicatesDFS(Node(.4, Node(.1, Empty, Empty), Node(.4, Empty, Empty)))
      == Node(.4, Node(.1, Empty, Empty), Empty))

    println(removeDuplicatesDFS(Node(.1,
      Node(.7, Node(.4, Empty, Empty), Node(.7, Empty, Empty)),
      Node(.3, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))
      == Node(.1, Node(.7, Node(.4, Empty, Empty), Empty), Node(.3, Node(.6, Empty, Empty), Empty)))

    println(removeDuplicatesDFS(
      Node(.1,
        Node(.2, Node(.4, Empty, Empty), Node(.5, Empty, Empty)),
        Node(.3, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))
      == Node(.1,
        Node(.2, Node(.4, Empty, Empty), Node(.5, Empty, Empty)),
        Node(.3, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))

    println("TASK 5.2: using BFS")

    println(removeDuplicatesBFS(Empty) == Empty)

    println(removeDuplicatesBFS(Node(.4, Empty, Empty)) == Node(.4, Empty, Empty))

    println(removeDuplicatesBFS(Node(.3, Node(.4, Empty, Empty), Node(.4, Empty, Empty)))
      == Node(.3, Node(.4, Empty, Empty), Empty))

    println(removeDuplicatesBFS(Node(.4, Node(.1, Empty, Empty), Node(.4, Empty, Empty)))
      == Node(.4, Node(.1, Empty, Empty), Empty))

    println(removeDuplicatesBFS(Node(.1,
      Node(.7, Node(.4, Empty, Empty), Node(.7, Empty, Empty)),
      Node(.3, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))
      == Node(.1, Node(.7, Node(.3, Empty, Empty), Empty), Node(.4, Node(.6, Empty, Empty), Empty)))

    println(removeDuplicatesBFS(
      Node(.1,
        Node(.2, Node(.4, Empty, Empty), Node(.5, Empty, Empty)),
        Node(.3, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))
      == Node(.1,
         Node(.2, Node(.3, Empty, Empty), Node(.4, Empty, Empty)),
         Node(.5, Node(.6, Empty, Empty), Node(.7, Empty, Empty))))
  }

}
