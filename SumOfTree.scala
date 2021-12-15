package main.paradygmaty
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

object SumOfTree {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def normalSumOfTree(tree: BT[Int]): Int =
    def sumHelper(node: BT[Int]): Int =
      node match
        case Empty => 0
        case Node(elem, left, right) =>
          if left == Empty && right == Empty then elem
          else elem + sumHelper(left) + sumHelper(right)

    sumHelper(tree)


  def sumTreeParallel(tree: BT[Int], maxDepth: Int = 15): Int =
    def sumHelper(node: BT[Int], currentDepth: Int): Int =
      if currentDepth <= maxDepth then
        normalSumOfTree(node)
      else
        node match
          case Empty => 0
          case Node(elem, left, right) =>
            if left == Empty && right == Empty then elem
            else
              val result = for {
                leftR  <- Future { sumHelper(left, currentDepth-1) }
                rightR <- Future { sumHelper(right, currentDepth-1) }

              } yield ( leftR + rightR + elem)

              Await.result(result, Duration.Inf)


    sumHelper(tree, maxDepth)


  val random = Random

  def generateTree(depth: Int, minVal: Int = 0, maxVal:Int = 100): BT[Int] =

    def generateTreeHelper(depth: Int): BT[Int] =
      if depth == 1 then Node(random.between(minVal, maxVal), Empty, Empty)
      else Node(random.between(minVal, maxVal), generateTreeHelper(depth-1), generateTreeHelper(depth-1))

    if depth <= 0 then Empty
    else generateTreeHelper(depth)



}

