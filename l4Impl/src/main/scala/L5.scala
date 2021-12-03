import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import scala.util.Random

object L5 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  // Zad 1,2

  def convertDecimalTo(root : Int, number : Int) : List[Int] = {
    @tailrec
    def convertDecimalToIn(root : Int, number : Int, digits : List[Int]) : List[Int] = {
      number / root match {
        case 0 => number :: digits
        case _ => convertDecimalToIn(root, number / root, (number % root) :: digits)
      }
    }
    if root < 2 then throw new IllegalArgumentException
    convertDecimalToIn(root, number, List())
  }

  def convertDecimalToHexadecimal(number : Int): List[Int] = convertDecimalTo(16, number)

  // Zad 3
  def generateBinaryTree(levelsNumber : Int, maxValue : Int) : BT[Int] = {
    levelsNumber match {
      case 1 => Node(Random.nextInt(maxValue) + 1, Empty, Empty)
      case _ => Node (Random.nextInt(maxValue) + 1, generateBinaryTree(levelsNumber - 1, maxValue), generateBinaryTree(levelsNumber - 1, maxValue))
    }
  }

  // Zad 4
  def multiplyTreeElements(bt: BT[Int]) : Int = {
    def multiplyTreeElementsIn(node : BT[Int]) : Int = {
      node match {
        case Empty => 1
        case Node(value, leftNode, rightNode) => {
          value * multiplyTreeElementsIn(leftNode) * multiplyTreeElementsIn(rightNode)
        }
      }
    }
    multiplyTreeElementsIn(bt)
  }



  def printBT[A](bt: BT[A]): Unit = {
    @tailrec
    def printBTIn(queue: Queue[BT[A]]) : Unit = {
      if queue.nonEmpty then {
        queue.dequeue() match {
          case Node(value, left, right) =>
            print(value)
            print(" ")
            queue.enqueue(left)
            queue.enqueue(right)
            printBTIn(queue)
          case Empty => printBTIn(queue)
        }
      }
    }
    printBTIn(Queue[BT[A]](bt))
    print("\n")
  }

  // Zad 5

  def createTree[A](values : List[A], valuesLength : Int) : BT[A] = {
    values match {
      case h :: t =>
        val (firstList, secondList) = t.splitAt(valuesLength)
        Node(h, createTree(firstList, valuesLength / 2), createTree(secondList, valuesLength / 2))
      case _ => Empty
    }
  }

  def removeDuplicatesBFS[A](bt: BT[A]) : BT[A] = {
    @tailrec
    def getUniqueNodesBFS(queue: Queue[BT[A]], uniqueValues: Set[A]) : List[A] = {
      if queue.isEmpty then uniqueValues.toList
      else {
        queue.dequeue() match {
          case Node(value : A, left : BT[A], right : BT[A]) =>
            uniqueValues.add (value)
            queue.enqueue (left)
            queue.enqueue (right)
            getUniqueNodesBFS(queue, uniqueValues)
          case Empty => getUniqueNodesBFS(queue, uniqueValues)
        }
      }
    }
    val uniqueNodes = getUniqueNodesBFS(Queue[BT[A]](bt), Set())
    createTree(uniqueNodes, uniqueNodes.length)
  }

  def removeDuplicatesDFS[A](bt : BT[A]) : BT[A] = {
    @tailrec
    def getUniqueNodesDFS(stack : Stack[BT[A]], uniqueValues : Set[A]) : List[A] = {
      if stack.isEmpty then uniqueValues.toList
      else {
        stack.pop() match {
          case Node(value, left, right) =>
            uniqueValues.add(value)
            stack.push(left)
            stack.push(right)
            getUniqueNodesDFS(stack, uniqueValues)
          case Empty => getUniqueNodesDFS(stack, uniqueValues)
        }
      }
    }
    val uniqueNodes = getUniqueNodesDFS(Stack[BT[A]](bt), Set())
    createTree(uniqueNodes, uniqueNodes.length)
  }


  def main(args: Array[String]) = {
    println(convertDecimalToHexadecimal(31))
    println(convertDecimalTo(2, 10))


    val bt = generateBinaryTree(5, 15)
    printBT(bt)
    println(multiplyTreeElements(bt))
    printBT(removeDuplicatesBFS(bt))
    printBT(removeDuplicatesDFS(bt))
  }
}
