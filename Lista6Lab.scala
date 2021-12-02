import scala.annotation.tailrec
import scala.util.Random
import scala.collection.mutable.Stack
import scala.collection.mutable.Set
import scala.collection.mutable.Queue

object Lista6Lab {


  def convertToHex(decimalNumber: Int): List[Int] = {
    @tailrec
    def convertToHexInner(decimalNumber: Int, hexNumber: List[Int]): List[Int] = {
      if (decimalNumber == 0) then hexNumber
      else convertToHexInner(decimalNumber / 16, decimalNumber % 16 :: hexNumber)
    }

    if decimalNumber == 0 then List(0)
    else if decimalNumber < 0 then Nil
    else convertToHexInner(decimalNumber, Nil);
  }


  def convertToChosenSystem(decimalNumber: Int, system: Int): List[Int] = {
    @tailrec
    def convertToChosenSystemInner(decimalNumber: Int, hexNumber: List[Int]): List[Int] = {
      if (decimalNumber == 0) then hexNumber
      else convertToChosenSystemInner(decimalNumber / system, decimalNumber % system :: hexNumber)
    }

    if decimalNumber == 0 then List(0)
    else if (decimalNumber < 0) then Nil
    else if (system <= 0) then Nil
    else convertToChosenSystemInner(decimalNumber, Nil);
  }

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = Random()

  def generateRandomTree(depth: Int): BT[Double] = {
    def generateRandomTreeInner(depth: Int): BT[Double] = {
      if depth == 0 then Empty
      else Node(random.nextFloat(), generateRandomTreeInner(depth - 1), generateRandomTreeInner(depth - 1))
    }

    generateRandomTreeInner(depth)

  }

  def multTheThree(tree: BT[Double]): Double = {
    def multTheThreeInner(tree: BT[Double]): Double = {
      tree match {
        case Empty => 1
        case Node(elem, left, right) => elem * multTheThreeInner(left) * multTheThreeInner(right)
      }
    }

    if isTreeAnImpostor(tree) then 0
    else multTheThreeInner(tree);
  }


  //TODO change the name of the method
  def isTreeAnImpostor(tree: BT[Double]): Boolean = {
    tree match {
      case Empty => true
      case Node(elem, left, right) => false
    }
  }

  def replaceDuplicatesTwoWays(tree: BT[Double], mult: Float): BT[Double] = {

    /*
    //BFS
    def getDuplicates(uniqueValues: Set[Double], queue: Queue[BT[Double]], duplicates: Set[Double]): (Set[Double], Set[Double]) = {
      if queue.isEmpty then (duplicates, uniqueValues)
      else queue.dequeue() match {
        case Empty => getDuplicates(uniqueValues, queue, duplicates)
        case Node(elem, left, right) => {
          queue.enqueue(left, right)
          if uniqueValues.contains(elem) then getDuplicates(uniqueValues, queue, duplicates += elem)
          else getDuplicates(uniqueValues += elem, queue, duplicates)
        }
      }
    }
*/

    //DFS
    def getDuplicates(uniqueValues: Set[Double], stack: Stack[BT[Double]], duplicates: Set[Double]): (Set[Double], Set[Double]) = {
      if stack.isEmpty then (duplicates, uniqueValues)
      else stack.pop() match {
        case Empty => getDuplicates(uniqueValues, stack, duplicates)
        case Node(elem, left, right) => {
          stack.push(left)
          stack.push(right)
          if uniqueValues.contains(elem) then getDuplicates(uniqueValues, stack, duplicates += elem)
          else getDuplicates(uniqueValues += elem, stack, duplicates)
        }
      }


    }


    val (duplicates, uniqueValues) = getDuplicates(Set(), Stack(tree), Set())
    //val (duplicates, uniqueValues) = getDuplicates(Set(), Queue(tree), Set())

    def findReplacement(duplicates: Set[Double], uniqueValues: Set[Double]): Float = {
      val randomValue = random.nextFloat() * mult
      if duplicates.contains(randomValue) || uniqueValues.contains(randomValue) then findReplacement(duplicates, uniqueValues)
      else randomValue
    }

    def replaceDuplicates(tree: BT[Double], duplicates: Set[Double], uniqueValues: Set[Double]): BT[Double] = {
      tree match {
        case Empty => Empty
        case Node(elem, left, right) => val replacement = findReplacement(duplicates, uniqueValues)
          if duplicates.contains(elem) then {
            if uniqueValues.contains(elem) then {
              uniqueValues -= elem
              Node(elem, replaceDuplicates(left, duplicates += replacement, uniqueValues), replaceDuplicates(right, duplicates += replacement, uniqueValues))
            }
            else
              Node(replacement, replaceDuplicates(left, duplicates += replacement, uniqueValues), replaceDuplicates(right, duplicates += replacement, uniqueValues))
          }
          else Node(elem, replaceDuplicates(left, duplicates += replacement, uniqueValues), replaceDuplicates(right, duplicates += replacement, uniqueValues))
      }
    }

    replaceDuplicates(tree, duplicates, uniqueValues);


  }


  def main(args: Array[String]): Unit = {

    println("dex -> hex");
    println(convertToHex(31) == List(1, 15));
    println(convertToHex(0) == List(0));
    println(convertToHex(-5) == Nil);
    println(convertToHex(10000) == List(2, 7, 1, 0));


    println("\ndex -> binary")
    println(convertToChosenSystem(0, 2) == List(0));
    println(convertToChosenSystem(300, 2) == List(1, 0, 0, 1, 0, 1, 1, 0, 0));

    println("\ndex -> 8")
    println(convertToChosenSystem(240, 8) == List(3, 6, 0));
    println(convertToChosenSystem(1000, 8) == List(1, 7, 5, 0));
    println(convertToChosenSystem(68, 8) == List(1, 0, 4));

    println("\nmore tests")
    println(convertToChosenSystem(240, -3) == Nil);
    println(convertToChosenSystem(-2, 8) == Nil);
    println(convertToChosenSystem(-1, -5) == Nil);

    println("\ngenerateTree")
    println(generateRandomTree(0))
    println(generateRandomTree(1))
    println(generateRandomTree(3))

    println("\nmultiple tree")
    println(multTheThree(generateRandomTree(3)))
    println(multTheThree(Node(0.5, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty))) < 0.126)
    println(multTheThree(Node(0.5, Node(0.5, Empty, Empty), Node(0.5, Empty, Empty))) > 0.124)
    println(multTheThree(Node(0.1, Node(0.1, Empty, Empty), Node(0.2, Empty, Empty))) > 0.0019)
    println(multTheThree(Node(0.1, Node(0.1, Empty, Empty), Node(0.2, Empty, Empty))) < 0.0021)
    println(multTheThree(Node(0.1, Empty, Empty)) == 0.1)
    println(multTheThree(Empty) == 0)

    println("\nDelete duplicates")
    println(replaceDuplicatesTwoWays(Node(1.0, Node(2.0, Node(3.0, Empty, Empty), Node(3.0, Empty, Empty)), Node(2.0, Node(2.0, Empty, Empty), Node(1.0, Empty, Empty))), 10));
    println(replaceDuplicatesTwoWays(Node(0.1, Node(0.1, Empty, Empty), Node(0.1, Empty, Empty)), 1));
    println(replaceDuplicatesTwoWays(Node(-1.0, Node(2.0, Node(3.0, Empty, Empty), Node(3.0, Empty, Empty)), Node(2.0, Node(2.0, Empty, Empty), Node(1.0, Empty, Empty))), 10));
    println(replaceDuplicatesTwoWays(Node(500.0, Node(500.1, Node(500.2, Empty, Empty), Node(499.9, Empty, Empty)), Node(500.1, Node(500.2, Empty, Empty), Node(500.3, Empty, Empty))), 500));
  }
}



