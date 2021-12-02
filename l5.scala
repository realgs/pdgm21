import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {

    println(convertTo16(31))
    println(convertNumber(29, 2))

    println()
    println()

    val treeTest = Node(1.0 ,Node(2.0,Node(2.0,Leaf,Leaf),Node(4.0,Leaf,Node(6.0,Leaf,Leaf))) ,Node(3.0,Node(4.0,Leaf,Leaf),Node(5.0,Node(6.0,Leaf,Leaf),Leaf)))


    val tree = createTree(4)
    println("Przejście wszerz " + breadthPrint(tree))
    println("Iloczyn " + preorderProduct(tree))
    println()

    println("Przejście wszerz " + breadthPrint(treeTest))
    println("Iloczyn " + preorderProduct(treeTest))
    println()




    val listOfValuesBFS = ValuesWithoutDuplicatesBFS(treeTest)
    println("Lista wartosci: " + listOfValuesBFS)
    val treeWithoutDuplicatesBFS = createTreeFromValues(listOfValuesBFS)
    println("Przejscie wszerz: " +  breadthPrint(treeWithoutDuplicatesBFS))
    println()

    val listOfValuesDFS = ValuesWithoutDuplicatesDFS(treeTest)
    println("Lista wartosci: " + listOfValuesDFS)
    val treeWithoutDuplicatesDFS = createTreeFromValues(listOfValuesDFS)
    println("Przejscie wszerz: " +  breadthPrint(treeWithoutDuplicatesDFS))
    println()


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


def preorderProduct(tree: BinaryTree[Double]) =
  def prerderHelper(node: BinaryTree[Double]):Double =
    node match
      case Leaf => 1;
      case Node(value, left, right) => value * prerderHelper(left) * prerderHelper(right)

  prerderHelper(tree)

//numbers are printed with 2 dig after comma
def breadthPrint(tree: BinaryTree[Double]) =
  def breadthHelper(listOfNodes: List[BinaryTree[Double]]):List[Double] =
    listOfNodes match
      case Nil => Nil
      case Leaf :: tail => breadthHelper(tail)
      case Node(value, left , right) :: tail => (Math.floor(value * 100) / 100) :: breadthHelper(tail ::: (List(left,right)))

  breadthHelper(List(tree))




def ValuesWithoutDuplicatesBFS(tree: BinaryTree[Double]) =
  def removeDuplicates(queue: List[BinaryTree[Double]], values: List[Double]):List[Double] =
    queue match
      case Nil => values
      case Leaf :: tail => removeDuplicates(tail,values)
      case Node(value, left , right) :: tail => {
        if values.contains(value) then removeDuplicates(tail ::: (List(left,right)), values)
        else removeDuplicates(tail ::: (List(left,right)), value :: values)
      }

  removeDuplicates(List(tree),Nil).reverse




def ValuesWithoutDuplicatesDFS(tree: BinaryTree[Double]) =
  def removeDuplicates(list: List[BinaryTree[Double]], values: List[Double]):List[Double] =
    list match
      case Nil => values
      case Leaf :: tail => removeDuplicates(tail,values)
      case Node(value, left , right) :: tail => {
        if values.contains(value) then removeDuplicates(left :: right :: tail, values)
        else removeDuplicates(left :: right :: tail, value :: values)
      }

  removeDuplicates(List(tree),Nil).reverse


def createTreeFromValues(values: List[Double]): BinaryTree[Double] =
  values match
    case Nil => Leaf
    case value :: tail => {
      val(lower,upper) = tail.splitAt((tail.size + 1) / 2 )
      Node(value, createTreeFromValues(lower), createTreeFromValues(upper))
    }





