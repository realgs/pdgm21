import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object TreeSum {

  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](element: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  val r = scala.util.Random

  def generateBinaryTree(depth: Int): BinaryTree[Double] =
    depth match
      case 0 => Empty
      case _ => Node(r.nextFloat, generateBinaryTree(depth - 1), generateBinaryTree(depth - 1))


  def parallel[A, B](taskA: => A, taskB: => B): (A,B) =
    val future: Future[B] = Future { taskB }
    val a: A = taskA
    val b: B = Await.result(future, Duration.Inf)
    (a,b)

  def treeElementsSum(tree: BinaryTree[Double]): Double = 
    def treeElementsSumInner(toVisit: BinaryTree[Double]): Double =
      toVisit match
        case Empty => 0
        case Node(value, leftSubtree, rightSubtree) => value + treeElementsSumInner(leftSubtree) + treeElementsSumInner(rightSubtree)
    treeElementsSumInner(tree)
  

  def treeElementsSumFuture(tree: BinaryTree[Double], maxDepth: Int): Double =
    def treeElementsSumInner(toVisit: BinaryTree[Double], depth: Int): Double =
      if depth <= maxDepth then treeElementsSum(toVisit)
      else
        toVisit match
          case Empty => 0
          case Node(value, leftSubtree, rightSubtree) =>
            val result = for {
              left <- Future { treeElementsSumInner(leftSubtree, depth - 1) }
              right <- Future { treeElementsSumInner(rightSubtree, depth - 1) }
            } yield (left + right + value)
            Await.result(result, Duration.Inf)
    treeElementsSumInner(tree, maxDepth)


  def treeElementsSumParallel(tree: BinaryTree[Double], maxDepth: Int): Double =
    def treeElementsSumInner(toVisit: BinaryTree[Double], depth: Int): Double =
      if depth <= maxDepth then treeElementsSum(toVisit)
      else
        toVisit match
          case Empty => 0
          case Node(value, leftSubtree, rightSubtree) =>
            val (left, right) = parallel(treeElementsSumInner(leftSubtree, depth - 1), treeElementsSumInner(rightSubtree, depth - 1))
            left + right + value
    treeElementsSumInner(tree, maxDepth)
}
