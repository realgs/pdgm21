import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.annotation.tailrec
import ParallelComponents.*

object MinBTree
{
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val generateRandom  = scala.util.Random

  /*def generateTree(depth: Int): BT[Double] =
    def generateTreeBody(depth: Int): BT[Double] =
      depth match
        case 0 => Empty
        case _ => Node(generateRandom.nextFloat, generateTreeBody(depth - 1), generateTreeBody(depth - 1))
    if depth > 0 then generateTreeBody(depth) else Empty*/

  def generateTree(depth: Int): BT[Double] =
    depth match
      case 0 => Empty
      case _ => Node(generateRandom.nextFloat, generateTree(depth - 1), generateTree(depth - 1))

  def minTree(tree: BT[Double]): Double =
    tree match
      case Empty => 2
      case Node(value, leftSubtree, rightSubtree) => Math.min(value, Math.min(minTree(leftSubtree), minTree(rightSubtree)))

  /*def minTreeParallel(tree: BT[Double]): Double =
    tree match
      case Empty => 2
      case Node(value, leftSubtree, rightSubtree) =>  if (leftSubtree  == Empty && rightSubtree == Empty) then value
                                                      else
                                                                val min = for {
                                                                  searchLeftSubtree <- Future {minTreeParallel(leftSubtree)}
                                                                  searchRightSubtree <-  Future {minTreeParallel(rightSubtree)}
                                                                } yield (Math.min(searchLeftSubtree, searchRightSubtree))
                                                                Math.min(value, Await.result(min, Duration.Inf))          */

  def minTreeMixedFuture(tree: BT[Double], maxDepth: Int): Double =
    def minTreeHelper(node: BT[Double], depthLeft: Int): Double =
      if depthLeft <= 0 then minTree(node)
      else node match
        case Empty => 2
        case Node(value, leftSubtree, rightSubtree) =>
          val f1 = Future(minTreeHelper(leftSubtree, depthLeft - 1))
          val f2 = Future(minTreeHelper(rightSubtree, depthLeft - 1))
          val (valL, valR) = ParallelComponents.parallel(f1, f2)
          Math.min(value, Math.min(valL, valR))
    minTreeHelper(tree, maxDepth)

  def minTreeMixedParallel(tree: BT[Double], maxDepth: Int): Double =
    def minTreeHelper(node: BT[Double], depthLeft: Int): Double =
      if depthLeft <= 0 then minTree(node)
      else node match
        case Empty => 2
        case Node(value, leftSubtree, rightSubtree) =>
          val (valL, valR) = ParallelComponents.parallelName(minTreeHelper(leftSubtree, depthLeft - 1), minTreeHelper(rightSubtree, depthLeft - 1))
          Math.min(value, Math.min(valL, valR))
    minTreeHelper(tree, maxDepth)

  def minTreeParallel(tree: BT[Double]): Double =
    tree match
      case Empty => 2
      case Node(value, leftSubtree, rightSubtree) =>
        val min = for {
          searchLeftSubtree <- Future {
            minTree(leftSubtree)
          }
          searchRightSubtree <- Future {
            minTree(rightSubtree)
          }
        } yield (Math.min(searchLeftSubtree, searchRightSubtree))
        Math.min(value, Await.result(min, Duration.Inf))

}
