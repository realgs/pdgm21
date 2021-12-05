import scala.annotation.tailrec

object Main {

  //Zadanie 1
  def convertDecimalHex(decNumber: Int): List[Int] = {
    @tailrec
    def convertDecimalHexIn(decNumber: Int, hexList: List[Int]): List[Int] = {
      decNumber match {
        case 0 => hexList
        case _ => convertDecimalHexIn(decNumber / 16, decNumber % 16 :: hexList)
      }
    }

    convertDecimalHexIn(decNumber, Nil)
  }

  // zadanie 2
  def convertDecimalAny(decNumber: Int, numSystem: Int): List[Int] = {
    if decNumber < 0 then throw new Exception("liczba ma być większa od zera")
    else if numSystem < 0 then throw new Exception("system nie może być ujemny")
    else {
      @tailrec
      def convertDecimalAnyIn(decNumber: Int, numSystem: Int, SystemList: List[Int]): List[Int] = {
        decNumber match {
          case 0 => SystemList
          case _ => convertDecimalAnyIn(decNumber / numSystem, numSystem: Int, decNumber % numSystem :: SystemList)
        }
      }

      convertDecimalAnyIn(decNumber, numSystem, Nil)
    }
  }

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //zadanie 3
  def TreeGenerator(depth: Int): BT[Double] = {
    val random = scala.util.Random
    depth match {
      case 0 => Empty
      case _ => Node(random.nextFloat(), TreeGenerator(depth - 1), TreeGenerator(depth - 1))
    }
  }

  //zadanie 4
  def ProductTree(tree: BT[Double]): Double = {
    def ProductTreeIn(tree: BT[Double], acc: Double): Double = {
      tree match {
        case Empty => acc
        case Node(valule, left, right) => ProductTreeIn(left, ProductTreeIn(right, acc * valule))
      }
    }
    ProductTreeIn(tree, 1.0)
  }

  //zadanie 5
  def DFSDuplicateDestroyer[A](tree: BT[A]): BT[A] = {
    def DFSDuplicateDestroyerIn(stack: List[BT[A]], visitetList: List[A]): List[A] = {
      stack match
        case Nil => visitetList.reverse
        case Empty :: t => DFSDuplicateDestroyerIn(t, visitetList)
        case Node(act, left, right) :: t =>
          if !visitetList.contains(act) then DFSDuplicateDestroyerIn(left :: right :: t, act :: visitetList)
          else DFSDuplicateDestroyerIn(left :: right :: t, visitetList)
    }
    TreefromList(DFSDuplicateDestroyerIn(List(tree), Nil))
  }

  def TreefromList[A](list: List[A]): BT[A] = {
    list match {
      case Nil => Empty
      case h :: t => Node(h , TreefromList(t.splitAt(t.length/2)._1) ,  TreefromList(t.splitAt(t.length/2)._2))
    }
  }
  def BFSDuplicateDestroyer[A](tree: BT[A]): BT[A] = {
    def BFSDuplicateDestroyerIn(queue: List[BT[A]] , visitedList: List[A]): List[A] ={
      queue match {
        case Nil => visitedList.reverse
        case Empty :: t => BFSDuplicateDestroyerIn(t , visitedList)
        case Node(act , left , right)  :: t => if visitedList.contains(act) then BFSDuplicateDestroyerIn(t ::: left :: List(right) , visitedList)
        else BFSDuplicateDestroyerIn(t ::: left :: List(right) , act::visitedList)
      }
    }
    TreefromList(BFSDuplicateDestroyerIn(List(tree), Nil))
  }

  def main(args: Array[String]): Unit = {
    println(convertDecimalHex(31))
    println(convertDecimalAny(31,16))
   var a = TreeGenerator(2);
    val b = Node(8, Node(6, Node(2, Node(1,Empty,Empty),Empty),Node(7, Node(7,Empty,Empty),Node(8,Empty,Empty))), Node(9, Node(9, Empty, Empty),Node(10,Empty,Empty)))
    val y = DFSDuplicateDestroyer(b)
    val c = BFSDuplicateDestroyer(b)
  }
}

