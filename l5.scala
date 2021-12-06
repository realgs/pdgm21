//Monika Jung l5

import scala.annotation.tailrec
import scala.util.Random

class l5 {

  //task 1 and 2
  def changeNumberSystem (number:Int, systemNr:Int) : List[Int] =
    @tailrec
    def recChangeNumberSystem(number:Int, result:List[Int]):List[Int] =
      if number < 0 then throw new Exception("number smaller than zero")
      else if systemNr < 1 then throw new Exception("systemNR smaller than zero")
      else if number < systemNr then List(number):::result
      else recChangeNumberSystem(number / systemNr, List(number % systemNr) ::: result)
    recChangeNumberSystem(number,Nil)

  changeNumberSystem (30, 3) == List(1, 0, 1, 0)
  changeNumberSystem (10, 2) == List(1, 0, 1, 0)
  changeNumberSystem (20, 16) == List(1, 4)

  //___________________________________________________________________
  def showList[A](list: List[A]): Unit =
    @tailrec
    def recShowList[A](listTail: List[A]): Unit =
      if listTail.head == Nil then ()
      else
        print(listTail.head);
        if listTail.tail != Nil then {
          print(", ");
          recShowList(listTail.tail);
        }
    recShowList(list)
    println()

  def mergeTwoListsF[A] (listA:List[A], listB:List[A]) = {
    @tailrec
    def recMergeTwoListsF(listA: List[A], listB: List[A], resultList: List[A]): List[A] =
      if listB == Nil then resultList ::: listA
      else if listA == Nil then resultList ::: listB
      else recMergeTwoListsF (listB.tail, listA, resultList ::: List(listB.head))
    recMergeTwoListsF(listA, listB, Nil)
  }

  {
    sealed trait BTree[+A]
    case object Empty extends BTree[Nothing]
    case class Node[+A](elem: A, left: BTree[A], right: BTree[A]) extends BTree[A]
  }
  val randomNumber = scala.util.Random

  //task 3
  def createRandomTreeFloat(treeDepth:Int):BTree[Float] =
    def recCreateRandomTreeFloat(currTreeDepth:Int, resultTree:BTree[Float]):BTree[Float] =
      if currTreeDepth <0 || treeDepth < 0 then throw new Exception ("wrong depth!")
      else if currTreeDepth == treeDepth then Node(randomNumber.nextFloat(),Empty, Empty)
      else Node(randomNumber.nextFloat(), recCreateRandomTreeFloat(currTreeDepth+1, resultTree), recCreateRandomTreeFloat(currTreeDepth+1, resultTree))
    recCreateRandomTreeFloat(0,Empty)

  def createRandomTreeInt(treeDepth:Int):BTree[Int] =
    def recCreateRandomTreeInt(currTreeDepth:Int, resultTree:BTree[Int]):BTree[Int] =
      if currTreeDepth <0 || treeDepth < 0 then throw new Exception ("wrong depth!")
      else if currTreeDepth == treeDepth then Node(randomNumber.nextInt(20),Empty, Empty)
      else Node(randomNumber.nextInt(20), recCreateRandomTreeInt(currTreeDepth+1, resultTree), recCreateRandomTreeInt(currTreeDepth+1, resultTree))
    recCreateRandomTreeInt(0,Empty)

  def countNodes[A](tree:BTree[A]):Int =
    tree match
      case Empty                       => 0
      case Node(_,treeLeft, treeRight) => 1 + countNodes(treeLeft) + countNodes(treeRight)

  def preorderBTree[A](tree: BTree[A]):List[A] =
    tree match
      case Node(value,left,right) => mergeTwoListsF(mergeTwoListsF(List(value), preorderBTree(left)), preorderBTree(right))
      case Empty                  => Nil

  def postorderBTree[A](tree: BTree[A]):List[A] =
    tree match
      case Node(value,left,right) => mergeTwoListsF(mergeTwoListsF(postorderBTree(left), postorderBTree(right)), List(value))
      case Empty                  => Nil

  def inorderBTree[A](tree: BTree[A]):List[A] =
    tree match
      case Node(value,left,right) => mergeTwoListsF(mergeTwoListsF(inorderBTree(left),inorderBTree(right)), List(value))
      case Empty                  => Nil

  var tree1:BTree[Float] = createRandomTreeFloat(2)
  var tree2:BTree[Int] = createRandomTreeInt (1)
  inorderBTree(tree1)
  inorderBTree(tree2)

  //task 4
  def multiplyIntNodes (tree:BTree[Int]):Int =
    tree match
      case Empty                  => 1
      case Node(value,left,right) => value * multiplyIntNodes(left) * multiplyIntNodes(right)

  def multiplyFloatNodes (tree:BTree[Float]):Float =
    tree match
      case Empty                  => 1
      case Node(value,left,right) => value * multiplyFloatNodes(left) * multiplyFloatNodes(right)

  multiplyFloatNodes(tree1)
  multiplyIntNodes(tree2)

  //task 5
  def changeIntValueOfNode (tree:BTree[Int]):BTree[Int]=
    tree match
      case Empty                   => Empty
      case Node (value,left,right) => Node(randomNumber.nextInt(20),left,right)

  def changeFloatValueOfNode (tree:BTree[Float]):BTree[Float]=
    tree match
      case Empty                   => Empty
      case Node (value,left,right) => Node(randomNumber.nextFloat(),left,right)

  @tailrec
  def chceckIfElemBelongsToList[A] (list:List[A], elem:A):Boolean =
    if list == Nil then false
    else if list.head == elem then true
    else chceckIfElemBelongsToList(list.tail, elem)

  def addSubTreeRight[A](subTree:BTree[A],tree:BTree[A]):BTree[A] =
    tree match
      case Empty => subTree
      case Node (value, left, Empty) => Node (value, left, subTree)
      case Node (value, left, right) => addSubTreeRight(subTree, right)

  def addSubTreeLeft[A](subTree:BTree[A],tree:BTree[A]):BTree[A] =
    tree match
      case Empty => subTree
      case Node (value, Empty, right) => Node (value, subTree, right)
      case Node (value, left, right) => addSubTreeLeft(subTree, left)

  def removeSubTree[A](subTree:BTree[A],tree:BTree[A]):BTree[A] =
    if subTree == Empty then tree
    else
      def recRemoveSubTree[A](subTree:BTree[A],treeR:BTree[A]):BTree[A] =
        treeR match
          case Empty => Empty
          case Node (value, subTree, right) => addSubTreeLeft(Node(value,Empty,right), removeSubTree(treeR,tree))
          case Node (value, left, subTree) => addSubTreeRight(Node(value,left,Empty), removeSubTree(treeR,tree))
          case Node (value, left, right) => {
            recRemoveSubTree(subTree,right)
            recRemoveSubTree(subTree,left)
          }
      recRemoveSubTree(subTree,tree)

  def removeRepeatings[A](tree: BTree[A]):BTree[A] =
    def recRemoveRepeatings[A](tree: BTree[A], treeR: BTree[A], valuesList:List[A]):BTree[A] =
      val elem:[A]
      treeR match
        case Empty                           => tree
        case Node (value,leftSubT,rightSubT) => {
          if chceckIfElemBelongsToList(valuesList, value) then {
            recRemoveRepeatings(addSubTreeRight(rightSubT,addSubTreeLeft(leftSubT, removeSubTree(treeR, tree))),
              addSubTreeRight(rightSubT,addSubTreeLeft(leftSubT, removeSubTree(treeR, tree)))), valuesList)
          }
          else
            recRemoveRepeatings(tree, leftSubT, value::valuesList)
            recRemoveRepeatings(tree, rightSubT, value::valuesList)
        }
}
