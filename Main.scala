import scala.annotation.tailrec
import scala.util.Random
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

object Main {
  // Zadanie 1
  // Zagwarantowalem dobra zlozonosc pamieciowa i obliczeniowa dzieki:
  // 1. Uzycie :: (konstruktora listy niepustej)
  // 2. Dodawanie wartosci do listy w taki sposob, ze nie trzeba potem odwracac listy
  // 3. Uzywaniu rekursji ogonowej
  def convertFromDecimalToHex(decimalNumber: Int): List[Int] = {
    @tailrec
    def convertFromDecimalToHexInner(decimalNumber: Int, hexNumber: List[Int]): List[Int] = {
      if decimalNumber == 0 then hexNumber
      else convertFromDecimalToHexInner(decimalNumber / 16, decimalNumber % 16 :: hexNumber)
    }

    if decimalNumber == 0 then List(0)
    else convertFromDecimalToHexInner(decimalNumber, Nil)
  }

  def convertFromDecimalToOtherSystem(decimalNumberAndSystem: List[Int]): List[Int] = {
    def convertFromDecimalToOtherSystemInner(decimalNumber: Int, system: Int, number: List[Int]): List[Int] = {
      if decimalNumber == 0 then number
      else convertFromDecimalToOtherSystemInner(decimalNumber / system, system, decimalNumber % system :: number)
    }

    if decimalNumberAndSystem(0) == 0 then List(0)
    else convertFromDecimalToOtherSystemInner(decimalNumberAndSystem(0), decimalNumberAndSystem(1), Nil)
  }

  // Define tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  // Zadanie 3
  def generateTree(depth: Int): BT[Float] = {
    def generateTreeInner(depth: Int, currentDepth: Int): BT[Float] = {
      val rng = Random()
      if depth == currentDepth then Empty
      else Node(rng.nextFloat(), generateTreeInner(depth, currentDepth+1), generateTreeInner(depth, currentDepth+1))
    }
    generateTreeInner(depth, 0)
  }

  // Zadanie 4
  def multiplyKeysInTree(startNode: BT[Float]): Float = {
    def multiplyKeysInTreeInner(node: BT[Float]): Float = {
      node match {
        case Empty => 1
        case Node(key, leftNode, rightNode) => key * multiplyKeysInTreeInner(leftNode) *
          multiplyKeysInTreeInner(rightNode)
      }
    }
    if startNode == Empty then 0
    else multiplyKeysInTreeInner(startNode)
  }

  // Zadanie 5
  def removeDuplicatesDFS[A](startNode: BT[A]): BT[A] = {
    // The function will return all duplicates values (values that cannot be added to set)
    @tailrec
    def getDuplicates(uniqueValues: Set[A], stack: Stack[BT[A]], duplicates: Set[A]): Set[A] = {
      if stack.isEmpty then duplicates
      else stack.pop match {
        case Empty => getDuplicates(uniqueValues, stack, duplicates)
        case Node(key, leftNode, rightNode) => {
          if uniqueValues.add(key) then getDuplicates(uniqueValues, stack.push(leftNode, rightNode), duplicates)
          else getDuplicates(uniqueValues, stack.push(leftNode, rightNode), duplicates += key)
        }
      }
    }

    val duplicates = getDuplicates(Set(), Stack(startNode), Set())
    // Remove duplicates from the tree
    def removeDuplicates(node: BT[A]): BT[A] = {
      node match {
        case Empty => Empty
        case Node(key, leftNode, rightNode) =>
          if duplicates.contains(key) then {
            val lNode = findReplacement(node)
            val rNode = findReplacement(node)
            // We add keys to duplicates to avoid multiplying nodes later
            (lNode, rNode) match {
              case (Empty, Empty) => Empty
              case (Empty, Node(key, _, _)) => {
                duplicates.add(key)
                Node(key, removeDuplicates(leftNode), removeDuplicates(rightNode))
              }
              case (Node(key, _, _), _) => {
                duplicates.add(key)
                Node(key, removeDuplicates(leftNode), removeDuplicates(rightNode))
              }
            }
          }
          else Node(key, removeDuplicates(leftNode), removeDuplicates(rightNode))
      }
    }

    // Find a replacement value (we go down to the leaf while trying to avoid duplicate values)
    // Rewrite this function
    def findReplacement(node: BT[A]): BT[A] = {
      node match {
        case Empty => Empty
        case Node(key, leftNode, rightNode) => {
          val lNode = findReplacement(leftNode)
          val rNode = findReplacement(rightNode)
          if duplicates.contains(key) && lNode == Empty && rNode == Empty then Empty
          else if lNode == Empty && rNode == Empty then node
          else if lNode == Empty then rNode
          else lNode
        }

      }
    }

    removeDuplicates(startNode)
  }


  def removeDuplicatesBFS[A](startNode: BT[A]): BT[A] = {
    // The function will return all duplicates values (values that cannot be added to set)
    @tailrec
    def getDuplicates(uniqueValues: Set[A], queue: Queue[BT[A]], duplicates: Set[A]): Set[A] = {
      if queue.isEmpty then duplicates
      else queue.dequeue match {
          case Empty => getDuplicates(uniqueValues, queue, duplicates)
          case Node(key, leftNode, rightNode) => {
            if uniqueValues.add(key) then getDuplicates(uniqueValues, queue.enqueue(leftNode, rightNode), duplicates)
            else getDuplicates(uniqueValues, queue.enqueue(leftNode, rightNode), duplicates += key)
          }
      }
    }

    val duplicates = getDuplicates(Set(), Queue(startNode), Set())
    // Remove duplicates from the tree
    def removeDuplicates(node: BT[A]): BT[A] = {
      node match {
        case Empty => Empty
        case Node(key, leftNode, rightNode) =>
          if duplicates.contains(key) then {
            val lNode = findReplacement(node)
            val rNode = findReplacement(node)
            // We add keys to duplicates to avoid multiplying nodes later
            (lNode, rNode) match {
              case (Empty, Empty) => Empty
              case (Empty, Node(key, _, _)) => {
                duplicates.add(key)
                Node(key, removeDuplicates(leftNode), removeDuplicates(rightNode))
              }
              case (Node(key, _, _), _) => {
                duplicates.add(key)
                Node(key, removeDuplicates(leftNode), removeDuplicates(rightNode))
              }
            }
          }
          else Node(key, removeDuplicates(leftNode), removeDuplicates(rightNode))
      }
    }

    // Find a replacement value (we go down to the leaf while trying to avoid duplicate values)
    // Rewrite this function
    def findReplacement(node: BT[A]): BT[A] = {
      node match {
        case Empty => Empty
        case Node(key, leftNode, rightNode) => {
          val lNode = findReplacement(leftNode)
          val rNode = findReplacement(rightNode)
          if duplicates.contains(key) && lNode == Empty && rNode == Empty then Empty
          else if lNode == Empty && rNode == Empty then node
          else if lNode == Empty then rNode
          else lNode
        }

      }
    }

    removeDuplicates(startNode)
  }

  def main(args: Array[String]): Unit = {
    println(convertFromDecimalToHex(31) == List(1, 15))
    println(convertFromDecimalToHex(4096) == List(1, 0, 0, 0))
    println(convertFromDecimalToHex(0) == List(0))
    println()

    println(convertFromDecimalToOtherSystem(List(31, 16)) == List(1, 15))
    println(convertFromDecimalToOtherSystem(List(12, 3)) == List(1, 1, 0))
    println(convertFromDecimalToOtherSystem(List(0, 2)) == List(0))
    println()

    val tree1 = generateTree(1)
    val tree2 = generateTree(3)
    val tree3 = generateTree(0)

    println(tree1)
    println(tree2)
    println(tree3)
    println()

    println(multiplyKeysInTree(tree1))
    println(multiplyKeysInTree(tree2))
    println(multiplyKeysInTree(tree3))
    println()

    val tree4 = Node(1, Node(2, Node(3, Node(4, Node(10, Node(3, Empty, Empty), Node(15, Empty, Empty)),
      Node(11, Empty, Empty)), Node(7, Empty, Empty)), Node(9, Node(5, Empty, Empty), Node(6, Empty, Empty))),
      Node(3, Node(3, Empty, Empty), Empty))
    val tree5 = Node(1, Node(1, Empty, Empty), Empty)
    val tree6 = Empty

    println(removeDuplicatesDFS(tree4))
    println(removeDuplicatesDFS(tree5))
    println(removeDuplicatesDFS(tree6))
    println()
    println(removeDuplicatesBFS(tree4))
    println(removeDuplicatesBFS(tree5))
    println(removeDuplicatesBFS(tree6))
  }
}
