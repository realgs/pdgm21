package SumOfTree

import scala.util.Random
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import ParallelFunction.ParallelFunction.*

object SumOfTree {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = new Random()

  def generateTree(tree_depth: Int): BT[Int] =
  {
    if tree_depth <= 0 then Empty
    else Node(random.nextInt(1000000)-500000, generateTree(tree_depth-1), generateTree(tree_depth-1))
  }

  def breadthBT[A](tree: BT[A]): List[A] =
    def breadthBTHelper[A](queue: List[BT[A]]): List[A] =
      queue match {
        case Nil => Nil
        case Empty :: tail => breadthBTHelper(tail)
        case Node(value, l_subtree, r_subtree) :: tail => value :: breadthBTHelper(tail ::: List(l_subtree, r_subtree))
      }
    breadthBTHelper(List(tree))

  def treeSum(tree: BT[Int]): Int =
  {
    tree match {
      case Empty => 0
      case Node(value, left_subtree, right_subtree) => value + treeSum(left_subtree) + treeSum(right_subtree)
    }
  }

  def treeSumFuture(tree: BT[Int], depth_threshold: Int): Int =
    if depth_threshold <= 0 then treeSum(tree)
    else
    {
      tree match {
        case Empty => 0
        case Node(value, left_subtree, right_subtree) =>
        {
          val f1 = Future(treeSumFuture(left_subtree, depth_threshold-1))
          val f2 = Future(treeSumFuture(right_subtree, depth_threshold-1))
          val res = for {
            x <- f1
            y <- f2
          } yield x+y
          value + Await.result(res, Duration.Inf)
        }
      }
    }

  def treeSumParallel(tree: BT[Int], depth_threshold: Int): Int =
    if depth_threshold <= 0 then treeSum(tree)
    else
    {
      tree match {
        case Empty => 0
        case Node(value, left_subtree, right_subtree) =>
        {
          val (f1, f2) = parallel(treeSumParallel(left_subtree, depth_threshold-1), treeSumParallel(right_subtree, depth_threshold-1))
          value + f1 + f2
        }
      }
    }



}
