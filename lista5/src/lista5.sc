//zadanie 1
def toHexadecimalList(x: Int): List[Int] =
  def toHexadecimalListTail(x: Int, finalList: List[Int]): List[Int] =
    x match
      case 0 => finalList
      case _ => toHexadecimalListTail(x/16, (x%16)::finalList)
  toHexadecimalListTail(x, Nil)

toHexadecimalList(31)
toHexadecimalList(83)


//zadanie 2
def toAnotherSystemList(x: Int, system: Int): List[Int] =
  def toAnotherSystemListTail(x: Int, system: Int, finalList: List[Int]): List[Int] =
    x match
      case 0 => finalList
      case _ => toAnotherSystemListTail(x/system, system, (x%system)::finalList)
  toAnotherSystemListTail(x, system, Nil)

toAnotherSystemList(31, 2)
toAnotherSystemList(83, 8)


//zadanie 3
sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[+A](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A]

val r = new scala.util.Random
def generateTree(n: Int): Tree[Double] = {
  n match
    case 0 => Empty
    case _ => Node(r.nextDouble(), generateTree(n-1), generateTree(n-1))
}

def breadthTree[A] (tree: Tree[A]): List[A] =
  def search(queue: List[Tree[A]]): List[A] =
    queue match
      case Nil => Nil
      case Empty :: t => search(t)
      case Node(h, left, right) :: t => h::search(t ::: List(left, right))
  search(List(tree))

val tree = generateTree(3)
breadthTree(tree)


//zadanie 4
def productOfTree(tree: Tree[Double]): Double =
  def productofTreeTail(queue: List[Tree[Double]], result: Double): Double =
    queue match
      case Nil => result
      case Empty :: t => productofTreeTail(t, result)
      case Node(h, left, right) :: t => productofTreeTail(t ::: List(left, right), result*h)
  productofTreeTail(List(tree), 1)

productOfTree(tree)


//zadanie 5 - DFS
def deleteDuplicatesDFS[A](tree: Tree[A]): Tree[A] = {
  def search(stack: List[Tree[A]], visited: List[A]): List[A] =
    stack match
      case Nil => Nil
      case Empty :: t => search(t, visited)
      case Node(h, left, right) :: t => if visited contains h then search(List(left, right) ::: t,  h :: visited)
                                        else h::search(List(left, right) ::: t,  h :: visited)

  def createTreeFromListDFS[A](list: List[A]): Tree[A] =
    list match
      case Nil => Empty
      case h::t => Node(h, createTreeFromListDFS(t.splitAt(t.length/2)._1), createTreeFromListDFS(t.splitAt(t.length/2)._2))

  createTreeFromListDFS(search(List(tree), Nil))
}

def depthTree[A] (tree: Tree[A]): List[A] =
  def search(stack: List[Tree[A]]): List[A] =
    stack match
      case Nil => Nil
      case Empty :: t => search(t)
      case Node(h, left, right) :: t => h::search(List(left, right) ::: t)
  search(List(tree))


val tree2 = Node(1,
  Node(2,
    Node(3,
      Node(4,
        Empty,
        Empty),
      Node(5,
        Node(6,
          Empty,
          Empty),
        Empty)),
    Node(7,
      Empty,
      Empty)),
  Node(3,
    Node(7,
      Node(8,
        Empty,
        Empty),
      Node(9,
        Empty,
        Empty)),
    Empty))


depthTree(tree2)
val tree3 = deleteDuplicatesDFS(tree2)
depthTree(tree3)

val tree4 = Node(1,
  Node(2,
    Node(3,
      Node(4,
        Empty,
        Empty),
      Node(5,
        Empty,
        Empty)),
    Node(8,
      Empty,
      Empty)),
  Node(4,
    Node(7,
      Node(8,
        Empty,
        Empty),
      Node(9,
        Empty,
        Empty)),
    Empty))


depthTree(tree4)
val tree5 = deleteDuplicatesDFS(tree4)
depthTree(tree5)
