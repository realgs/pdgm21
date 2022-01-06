import scala.concurrent._, ExecutionContext.Implicits.global, duration._
import scala.util.Random

object Lista7_MaxElementInTree {

  def executionTime[A](task: => A): Long =
    val t0 = System.nanoTime()
    task
    val t1 = System.nanoTime()
    t1 - t0

  def parallel[A,B](taskA: => A, taskB: => B): (A,B) =
    val right = Future {taskB}
    val left = taskA
    (left, Await.result(right,Duration.Inf))

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = Random
  def generateBTree(depth: Int) =
    def helper(depthLeft: Int): BT[Double] =
      depthLeft match
        case 0 => Empty
        case _ => Node(random.nextDouble(), helper(depthLeft - 1), helper(depthLeft - 1))
    if depth <= 0 then Empty else helper(depth)

  // treeDepth > 26 leads to stackOverflow (2^26 - 1 = 67108863 element)
  def maxElementInTree (tree: BT[Double]) =
    def helper (currentTree: BT[Double]): Double =
      currentTree match
        case Node(elem, Empty, Empty) => elem
        case Node(elem, leftTree, rightTree) => math.max(elem, math.max(helper(leftTree), helper(rightTree)))
        case Empty => throw new Exception("It should never happend !")
    helper(tree)

  //depthToSplit > 10 leads to not ending function
  def parallelMaxElementInTree (tree: BT[Double], depthToSplit: Int) =
    def helper (currentTree: BT[Double], depthToSplitLeft: Int): Double =
      if depthToSplitLeft <= 0 then maxElementInTree(currentTree)
      else currentTree match
        case Node(elem, Empty, Empty) => elem
        case Node(elem, leftTree, rightTree) =>
          val (leftMAX, rightMax) = parallel(helper(leftTree, depthToSplitLeft - 1), helper(rightTree, depthToSplitLeft - 1))
          Math.max(elem, Math.max(leftMAX, rightMax))
        case Empty => throw new Exception("It should never happend !")
    helper(tree, depthToSplit)

  def main(args: Array[String]): Unit = {

    val tree1 = generateBTree(5)
    val tree2 = generateBTree(10)
    val tree3 = generateBTree(15)
    val tree4 = generateBTree(20)
    val tree5 = generateBTree(25)

    var splitDepth = 1
    println("\nSplit depth: 1")
    println("\nTree depth: 5")
    println("Normal method:   " + executionTime(maxElementInTree(tree1) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree1, splitDepth) + " ns"))
    println("\nTree depth: 10")
    println("Normal method:   " + executionTime(maxElementInTree(tree2) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree2, splitDepth) + " ns"))
    println("\nTree depth: 15")
    println("Normal method:   " + executionTime(maxElementInTree(tree3) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree3, splitDepth) + " ns"))
    println("\nTree depth: 20")
    println("Normal method:   " + executionTime(maxElementInTree(tree4) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree4, splitDepth) + " ns"))
    println("\nTree depth: 25")
    println("Normal method:   " + executionTime(maxElementInTree(tree5) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree5, splitDepth) + " ns"))

    splitDepth = 2
    println("\nSplit depth: 2")
    println("\nTree depth: 5")
    println("Normal method:   " + executionTime(maxElementInTree(tree1) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree1, splitDepth) + " ns"))
    println("\nTree depth: 10")
    println("Normal method:   " + executionTime(maxElementInTree(tree2) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree2, splitDepth) + " ns"))
    println("\nTree depth: 15")
    println("Normal method:   " + executionTime(maxElementInTree(tree3) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree3, splitDepth) + " ns"))
    println("\nTree depth: 20")
    println("Normal method:   " + executionTime(maxElementInTree(tree4) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree4, splitDepth) + " ns"))
    println("\nTree depth: 25")
    println("Normal method:   " + executionTime(maxElementInTree(tree5) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree5, splitDepth) + " ns"))

    splitDepth = 3
    println("\nSplit depth: 3")
    println("\nTree depth: 5")
    println("Normal method:   " + executionTime(maxElementInTree(tree1) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree1, splitDepth) + " ns"))
    println("\nTree depth: 10")
    println("Normal method:   " + executionTime(maxElementInTree(tree2) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree2, splitDepth) + " ns"))
    println("\nTree depth: 15")
    println("Normal method:   " + executionTime(maxElementInTree(tree3) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree3, splitDepth) + " ns"))
    println("\nTree depth: 20")
    println("Normal method:   " + executionTime(maxElementInTree(tree4) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree4, splitDepth) + " ns"))
    println("\nTree depth: 25")
    println("Normal method:   " + executionTime(maxElementInTree(tree5) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree5, splitDepth) + " ns"))

    splitDepth = 4
    println("\nSplit depth: 4")
    println("\nTree depth: 5")
    println("Normal method:   " + executionTime(maxElementInTree(tree1) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree1, splitDepth) + " ns"))
    println("\nTree depth: 10")
    println("Normal method:   " + executionTime(maxElementInTree(tree2) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree2, splitDepth) + " ns"))
    println("\nTree depth: 15")
    println("Normal method:   " + executionTime(maxElementInTree(tree3) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree3, splitDepth) + " ns"))
    println("\nTree depth: 20")
    println("Normal method:   " + executionTime(maxElementInTree(tree4) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree4, splitDepth) + " ns"))
    println("\nTree depth: 25")
    println("Normal method:   " + executionTime(maxElementInTree(tree5) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree5, splitDepth) + " ns"))

    splitDepth = 5
    println("\nSplit depth: 5")
    println("\nTree depth: 5")
    println("Normal method:   " + executionTime(maxElementInTree(tree1) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree1, splitDepth) + " ns"))
    println("\nTree depth: 10")
    println("Normal method:   " + executionTime(maxElementInTree(tree2) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree2, splitDepth) + " ns"))
    println("\nTree depth: 15")
    println("Normal method:   " + executionTime(maxElementInTree(tree3) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree3, splitDepth) + " ns"))
    println("\nTree depth: 20")
    println("Normal method:   " + executionTime(maxElementInTree(tree4) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree4, splitDepth) + " ns"))
    println("\nTree depth: 25")
    println("Normal method:   " + executionTime(maxElementInTree(tree5) + " ns"))
    println("Parallel method: " + executionTime(parallelMaxElementInTree(tree5, splitDepth) + " ns"))
  }
}
