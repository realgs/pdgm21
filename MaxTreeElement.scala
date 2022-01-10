import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

object MaxTreeElement {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = Random
  def generateBTree(depth: Int): BT[Double] =
    def helper(depthLeft: Int): BT[Double] =
      depthLeft match
        case 0 => Empty
        case _ => Node(random.nextDouble(), helper(depthLeft - 1), helper(depthLeft - 1))
    if depth <= 0 then Empty else helper(depth)

  def normalMaxElementInTree (tree: BT[Double]): Double =
    def helper (currentTree: BT[Double]): Double =
      currentTree match
        case Node(elem, Empty, Empty) => elem
        case Node(elem, leftTree, rightTree) => math.max(elem, math.max(helper(leftTree), helper(rightTree)))
        case Empty => throw new Exception("It should never happend !")
    helper(tree)

  def parallelMaxElementInTree (tree: BT[Double], depthToSplit: Int): Double =
    def helper (currentTree: BT[Double], depthToSplitLeft: Int): Double =
      if depthToSplitLeft <= 0 then normalMaxElementInTree(currentTree)
      else currentTree match
        case Node(elem, Empty, Empty) => elem
        case Node(elem, leftTree, rightTree) =>
          val futureRight = Future {helper(rightTree, depthToSplitLeft - 1)}          
          val leftMax = helper(leftTree, depthToSplitLeft - 1)
          val rightMax = Await.result(futureRight, Duration.Inf)          
          math.max(elem, math.max(leftMax, rightMax))
        case Empty => throw new Exception("It should never happend !")
    helper(tree, depthToSplit)
}
