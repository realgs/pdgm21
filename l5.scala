import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {

    println(convertTo16(31))
    println(convertNumber(31, 16))

    val treeTest = Node(1.0, Node(2.0,Leaf,Leaf), Node(3.0,Leaf,Leaf))
    val tree = createTree(4)

    println(breadthPrint(treeTest))
    println(inorderProduct(treeTest))

    println(breadthPrint(tree))
    println(inorderProduct(tree))




  }
}



def convertTo16(number: Int) =
  @tailrec
  def converNumberHelper(value: Int, resultList: List[Int]): List[Int] =
    if(value > 0) then converNumberHelper((value / 16), (value % 16)::resultList)
    else resultList

  converNumberHelper(number,Nil)


def convertNumber(number: Int, base: Int) =
  @tailrec
  def converNumberHelper(value: Int, resultList: List[Int]): List[Int] =
    if(value > 0) then converNumberHelper((value / base), (value % base)::resultList)
    else resultList

  converNumberHelper(number,Nil)




sealed trait BinaryTree[+A]
case object Leaf extends BinaryTree[Nothing]
case class Node[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]


def getRandomNumber() =
  Math.floor(Math.random() * 100) / 100



def createTree(height :Int):BinaryTree[Double] =
  def createTreeNext(currentHeight :Int):BinaryTree[Double] =
    if(currentHeight == height) then Leaf
    else Node(Math.random(), createTreeNext(currentHeight + 1), createTreeNext(currentHeight + 1))

  createTreeNext(0)


def inorderProduct(tree: BinaryTree[Double]) =
  def inorderHelper(node: BinaryTree[Double]):Double =
    node match
      case Leaf => 1;
      case Node(value, left, right) => value * inorderHelper(left) * inorderHelper(right)

  inorderHelper(tree)


//numbers are printed with 2 dig after comma
def breadthPrint(tree: BinaryTree[Double]) =
  def breadthHelper(listOfNodes: List[BinaryTree[Double]]):List[Double] =
    listOfNodes match
      case Nil => Nil
      case Leaf :: tail => breadthHelper(tail)
      case Node(value, left , right) :: tail => (Math.floor(value * 100) / 100) :: breadthHelper(tail ::: (List(left,right)))

  breadthHelper(List(tree))

















