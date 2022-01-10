import scala.util.Random
import scala.concurrent._
import ExecutionContext.Implicits.global
import duration._
import scala.annotation.tailrec
import Functions.*

object Tree {

  sealed trait Tree[+A]
  case class Node[+A]( value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  case object EmptyNode extends Tree[Nothing]
  
  def randomTree(depth: Int): Tree[Double] =
    val random = new Random()
    def inner(depth: Int): Tree[Double] =
      if depth == 0 then EmptyNode
      else Node(random.nextFloat(), randomTree(depth - 1), randomTree(depth - 1))
    inner(depth)
    
  def randomTreePar(depth: Int, depthToPar: Int): Tree[Double] =
    if depthToPar > depth then EmptyNode
    else
      val random = new Random()
      def inner(depth: Int, depthToPar: Int): Tree[Double] =
        (depth, depthToPar) match
          case (_, 0) => randomTree(depth)
          case (0, _) => EmptyNode
          case _ => 
            val (left, right) = parallel(inner(depth - 1, depthToPar - 1), inner(depth - 1, depthToPar - 1))
            Node(random.nextFloat(), left, right)
      inner(depth, depthToPar)
    
  def sumTreeSeq(tree: Tree[Double]): Double =
    def inner(tree: Tree[Double]): Double =
      tree match
        case Node(value, left, right) => value + inner(left) + inner(right)
        case EmptyNode => 0
    inner(tree)

  def sumTreePar(tree: Tree[Double], maxDepth: Int): Double =
    def inner(tree: Tree[Double], depth: Int): Double =
      (depth, tree) match
        case (0, Node(va, left, right)) =>
          val (x, y) = parallel(sumTreeSeq(left), sumTreeSeq(right))
          va + x + y
        case (_, Node(va, left, right)) => va + inner( left,depth - 1) + inner(right, depth - 1)
        case (_, EmptyNode) => 0
    inner(tree, maxDepth)
    
  def deepenSeq(tree: Tree[Double]): Tree[Double] =
      tree match
        case Node(value, left, right) => Node(value, deepenSeq(left), deepenSeq(right))
        case EmptyNode => randomTree(1)
  
  def deepenPar(tree: Tree[Double], maxDepth: Int): Tree[Double] =
    def inner(depth: Int, tree: Tree[Double]): Tree[Double] =
      (depth, tree) match
          case (0, Node(va, left, right)) => 
            val (x, y) = parallel(deepenSeq(left), deepenSeq(right))
            Node(va, x, y)
          case (_, Node(va, left, right)) => Node(va,inner(depth - 1, left), inner(depth - 1, right) )
          case (_, EmptyNode) => randomTree(1)
    inner(maxDepth,tree)
  
  def depthSearch(bt: Tree[Double]): List[Double] =
    def inner[Double](queue: List[Tree[Double]]): List[Double] =
      queue match
        case Node(v, t1, t2) :: t => v :: inner(t1 :: t2 :: t)
        case EmptyNode :: t => inner(t)
        case Nil => Nil
    inner(List(bt))


  def getDepth(tree: Tree[Double]): Int =
    @tailrec
    def inner(result: Int, tree: Tree[Double]): Int =
      tree match
        case Node(_, left, _) => inner(result + 1, left)
        case EmptyNode => result
    inner(0, tree)
}