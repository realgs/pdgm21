//Maciej Olejnik 260444

//FIRST TASK *******************

// O(n)
//where n is the number of possible rounded down
//divisions in form of (number / 16) which arent 0

def changeNumberSystemToHex(number: Int): List[Int] =
  def changeNumberSystemInner(number: Int, list: List[Int]): List[Int] =
    if number == 0 then list
    else changeNumberSystemInner(number / 16, number % 16 :: list)
  changeNumberSystemInner(number, List())

// O(n)
//where n is the number of possible rounded down
//divisions in form of (number / system) which arent 0

def changeNumberSystem(number: Int, system: Int): List[Int] =
  def changeNumberSystemInner(number: Int, list: List[Int], system: Int): List[Int] =
    if system == 0 then throw new Exception("Number system cannot be zero!")
    if system < 0 then -1 :: changeNumberSystemInner(-number, list, system)
    else if number == 0 then list
    else changeNumberSystemInner(number / system, number % system :: list, system)
  changeNumberSystemInner(number, List(), system)

changeNumberSystem(31, 16)
changeNumberSystemToHex(31)
changeNumberSystem(31, 10)
changeNumberSystem(31, 2)
// changeNumberSystem(31, 0) Exception
changeNumberSystem(0, 2)
changeNumberSystem(31, 31)

// SECOND TASK *******************
val random = scala.util.Random

sealed trait BT[+T]
case object BTEmpty extends BT[Nothing]
case class BTNode[+T](elem: T, left: BT[T], right: BT[T]) extends BT[T]

// will assume this is O(1) - couldn't find any info about this method's complexity

def roundToDecimal(number: Float, place: Int): Float =
  BigDecimal(number).setScale(place, BigDecimal.RoundingMode.HALF_UP).toFloat

// O(2^(n+1) - 1) where n is the input n

def generateBinaryTree(n: Int): BT[Float] =
  if n == 0 then
    BTNode(roundToDecimal(random.nextFloat(), 2), BTEmpty, BTEmpty)
  else
    BTNode (roundToDecimal(random.nextFloat(), 2), generateBinaryTree(n - 1), generateBinaryTree(n - 1));;

val tree = generateBinaryTree(2)
val secondTree = generateBinaryTree(0)

// THIRD TASK *******************

// O(2n) where n is the amount of nodes in the tree

def multiplyTreeElements(tree: BT[Float]): Float =
  tree match
    case BTEmpty => 1
    case BTNode(value, left, right) => roundToDecimal(value * multiplyTreeElements(left) * multiplyTreeElements(right), 5)

multiplyTreeElements(tree)
multiplyTreeElements(secondTree)

// FOURTH TASK *******************

def isVisited[T](visitedList: List[T], queried: T): Boolean =
  if visitedList != Nil then
    if visitedList.head != queried then
      isVisited(visitedList.tail, queried)
    else true
  else false

def breadthDeleteDuplicates[T](tree: BT[T]): List[T] =
    def breadthDeleteDuplicatesInner[T](visitedList: List[T], imitatedQueue: List[BT[T]]): List[T] =
      imitatedQueue match
        case Nil => Nil
        case BTEmpty :: tail => breadthDeleteDuplicatesInner(visitedList, tail)
        case BTNode(value, left, right) :: tail =>
          if isVisited(visitedList, value) then breadthDeleteDuplicatesInner(visitedList, tail ::: List(left, right))
          else value :: breadthDeleteDuplicatesInner(value :: visitedList, tail ::: List(left, right))

    breadthDeleteDuplicatesInner(Nil, List(tree))


def breadthForTesting[T](tree: BT[T]): List[T] =
  def breadthForTestingInner[T](imitatedQueue: List[BT[T]]): List[T] =
    imitatedQueue match
      case Nil => Nil
      case BTEmpty :: tail => breadthForTestingInner(tail)
      case BTNode(value, left, right) :: tail => value :: breadthForTestingInner(tail ::: List(left, right))

  breadthForTestingInner(List(tree))


def depthDeleteDuplicate[T](tree: BT[T]): List[T] =
  def depthDeleteDuplicateInner[T](visitedList: List[T], imitatedStack: List[BT[T]]): List[T] =
    imitatedStack match
      case Nil => Nil
      case BTEmpty :: tail => depthDeleteDuplicateInner(visitedList, tail)
      case BTNode(value, left, right) :: tail =>
        if isVisited(visitedList, value) then depthDeleteDuplicateInner(visitedList, left :: right :: tail)
        else value :: depthDeleteDuplicateInner(value :: visitedList, left :: right :: tail)

  depthDeleteDuplicateInner(Nil, List(tree))

def depthForTesting[T](tree: BT[T]): List[T] =
  def depthForTestingInner(imitatedStack: List[BT[T]]): List[T] =
    imitatedStack match
      case Nil => Nil
      case BTEmpty :: tail => depthForTestingInner(tail)
      case BTNode(value, left, right) :: tail => value :: depthForTestingInner(left :: right :: tail)

  depthForTestingInner(List(tree))

val treeWithDuplicates = BTNode(1, BTNode(2, BTNode(3, BTEmpty, BTEmpty), BTNode(2, BTEmpty, BTEmpty)), BTNode(4, BTNode(3, BTEmpty, BTEmpty), BTEmpty))

breadthForTesting(treeWithDuplicates)
breadthDeleteDuplicates(treeWithDuplicates)

depthForTesting(treeWithDuplicates)
depthDeleteDuplicate(treeWithDuplicates)

/*
sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[+A](elem: A, children: Array[Tree[A]], parent: Tree[A]) extends Tree[A]
def BFS[A](tree: Tree[A]) =
  def iterateOverTreeBFS[A] (queue: List[Tree[A]], visited: List[Tree[A]], ): List[A] =
      if queue == List() then List()
      else
        queue.head match
          case Empty => iterateOverTreeBFS(queue.tail, visited)
          case Node(el, array, parent) =>
            if checkForVisited(Node(el, array, parent), visited) then
              iterateOverTreeBFS(queue.tail, visited)
            else
              iterateOverTreeBFS(queue.tail ::: array.toList, Node(el, array) :: visited)
  tree match
    case Empty => List()
    case Node(el, array) => iterateOverTreeBFS(array.toList, List(Node(el, array)), Node(el, array))

def checkForVisited[A](tree: Tree[A], visited: List[Tree[A]]): Boolean =
  def deleteChildFromParent[A](child: Tree[A]): Boolean =
    child match
      case Node(el, c_array, parent) =>
        parent match
          case Node(_, array, _) =>
            array.map(e =>
              if e == Node(el, c_array, parent)
                Empty
              else
            )
    false

  visited match
    case List() => false
    case h::t =>
      if h == tree then
        tree match
          case Node(el, array, parent) => deleteChildFromParent(Node(el, array, parent))
      else
        checkForVisited(tree, t)
*/
