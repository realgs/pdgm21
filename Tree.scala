import scala.annotation.tailrec

class Node[T](var value: T, val size: Int) {

  private val kids = new Array[Node[T]](size)
  private var index = 0

  def getValue:T = value

  def setValue(newValue: T): Unit = value = newValue

  def getKids: Array[Node[T]] = kids

  def addKid(newKid: Node[T]): Unit =
    kids(index) = newKid
    index += 1

  def addNewKid(value: T): Unit =
    kids(index) = new Node[T](value, kids.length)

  def addKidAt(newKid: Node[T], index: Int): Unit =
    kids(index) = newKid

  def addNewKidAt(value: T, index: Int): Unit =
    kids(index) = new Node[T](value, kids.length)
}

class Tree(val kidsLimit: Int){

  var root: Node[Int] = Node[Int](0, kidsLimit)

  def getNode(path: List[Int]): Node[Int] =
    @tailrec
    def iterate(curr: Node[Int], tail: List[Int]): Node[Int] =
      tail match
        case Nil => curr
        case h::t => iterate(curr.getKids(h-1), t)
    iterate(root, path)


}

