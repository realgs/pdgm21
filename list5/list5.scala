import scala.util.Random
import scala.annotation.tailrec

object Lista5 {
    // function that converts any number to hexadecimal
    // output is a list of hexadecimal integers
    def convertDecToHex(num: Int): List[Int] =
        if num == 0 then List(0)
        else if num < 0 then throw new IllegalArgumentException("negative numbers are not allowed")
        else
            @tailrec
            def convertDecToHexTail(num: Int, acc: List[Int]): List[Int] =
                if num == 0 then acc
                else convertDecToHexTail(num / 16, (num % 16) :: acc)
            convertDecToHexTail(num, Nil)

    // function that converts any number to custom base
    // output is list of digits
    def convertDecToAny(num: Int, base: Int): List[Int] =
        if num == 0 then List(0)
        else if num < 0 then throw new IllegalArgumentException("negative numbers are not allowed")
        else
            @tailrec
            def convertDecToAnyTail(num: Int, acc: List[Int]): List[Int] =
                if num == 0 then acc
                else convertDecToAnyTail(num / base, (num % base) :: acc)
            convertDecToAnyTail(num, Nil)

    sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](value: A, left: BT[A], right: BT[A]) extends BT[A]

    val valueRange = 9
    def generateTree(depth: Int): BT[Int] =
        val value = Random.between(1, valueRange)
        if(depth < 0) throw new IllegalArgumentException("negative numbers are not allowed")
        else if (depth > 0) Node(value, generateTree(depth - 1), generateTree(depth - 1))
        else Node(value, Empty, Empty)

    // function that prints a binary tree in a preorder fashion
    // all values should be printed in the same line separated by a space
    // line should start with a "[" and end with a "]"
    def printTree(tree: BT[Int]): Unit =
        print("Tree: [")
        def printTreeInternal(tree: BT[Int]): Unit =
            tree match
                case Empty => ()
                case Node(value, left, right) =>
                    print(value)
                    print(" ")
                    printTreeInternal(left)
                    printTreeInternal(right)
        printTreeInternal(tree)
        println("]")

    // Zadanie 4
    def multiply(tree: BT[Int]): Int =
        tree match
            case Empty => 1
            case Node(value, left, right) => value * multiply(left) * multiply(right)

    // Zadanie 5
    def listToTree[A](listOfNodes: List[A]): BT[A] =
        // Given a list of values, recreate a binary tree
        listOfNodes match
            case Nil => Empty
            case list =>  val splitLists = list.tail.splitAt(list.tail.size / 2)
                Node(list.head, listToTree(splitLists._1), listToTree(splitLists._2))

    def removeDuplicatesBfs[A](tree: BT[A]): BT[A] =
        // Remove duplicates in a tree using BFS
        @tailrec
        def treeToQueue(queue: List[BT[A]], visitedValues: List[A]): List[A] =
            // Converts tree to queue, removes duplicates and returns list of values
            queue match
                case Nil => visitedValues
                case Empty :: t => treeToQueue(t, visitedValues)
                case Node(value, lTree, rTree) :: t =>  if visitedValues contains value then treeToQueue(t ::: List(lTree, rTree), visitedValues)
                                                        else treeToQueue(t ::: List(lTree, rTree), value :: visitedValues)
        listToTree(treeToQueue(tree :: Nil, Nil).reverse)

    def removeDuplicatesDfs[A](tree: BT[A]): BT[A] =
        // Remove duplicates in a tree using DFS
        def walk(queue: BT[A], visitedValues: List[A]): List[A] =
            // Walks through tree and removes duplicates
            queue match
                case Empty => visitedValues
                case Node(value, leftSubtree, rightSubtree) =>  if visitedValues contains value then walk(rightSubtree, walk(leftSubtree, visitedValues))
                                                                else walk(rightSubtree, walk(leftSubtree, value :: visitedValues))
        listToTree(walk(tree, Nil).reverse)

    def main(args: Array[String]): Unit = {
        println("Convert dec to hex tests: ")
        println(convertDecToHex(0) == List(0))
        println(convertDecToHex(16) == List(1, 0))
        println(convertDecToHex(31) == List(1, 15))

        println("Convert dec to any tests: ")
        println(convertDecToAny(10, 2) == List(1, 0, 1, 0))
        println(convertDecToAny(10, 8) == List(1, 2))
        println(convertDecToAny(31, 16) == List(1, 15))

        println("List to tree test: ")
        val listOfValues = List(1, 2, 3, 4, 5, 6, 7)
        val listToTreeTest = listToTree(listOfValues)
        println(listToTreeTest)

        println("Generate tree test")
        val tree = generateTree(3)
        println(tree)
        println("Multiply test: ")
        println(multiply(tree))

        val bfstree = removeDuplicatesBfs(tree)
        val dfstree = removeDuplicatesDfs(tree)

        println("Remove duplicates BFS: ")
        println(bfstree)
        println("Remove duplicates DFS: ")
        println(dfstree)
    }
}
