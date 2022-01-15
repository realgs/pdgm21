import scala.annotation.tailrec

class Node[T](var value: T, val size: Int) {

  private val kids = new Array[Node[T]](size)
  private var index = 0

  def getValue:T = value

  def setValue(newValue: T): Unit = value = newValue

  def hasKids: Boolean = index != 0

  def getKids: Array[Node[T]] = kids

  def getIndex: Int = index

  def addKid(newKid: Node[T]): Unit =
    kids(index) = newKid
    if index == -1 then index = 1
    else index += 1

  def addNewKid(value: T): Unit =
    kids(index) = new Node[T](value, kids.length)
    if index == -1 then index = 1
    else index += 1

  def addKidAt(newKid: Node[T], index: Int): Unit =
    kids(index) = newKid
    if this.index == 0 then this.index = -1

  def addNewKidAt(value: T, index: Int): Unit =
    kids(index) = new Node[T](value, kids.length)
    if this.index == 0 then this.index = -1

  def findPaths(): Array[(T, List[Int])] =
    var res = new Array[(T, List[Int])](0)
    def search(curr: Node[T], path: List[Int]): Unit =
      if curr.hasKids then
        var i = 0
        while(i < size){
          res = res:+(curr.getKids(i).getValue, path:+i)
          search(curr.getKids(i), path:+i)
          i += 1
        }
    search(this,List())
    res  
}

class MyTree(val kidsLimit: Int, game: Kalaha){

  var root: Node[(Int, Kalaha)] = Node[(Int, Kalaha)]((0, game), kidsLimit)

  def getNode(path: List[Int]): Node[(Int, Kalaha)] =
    @tailrec
    def iterate(curr: Node[(Int, Kalaha)], tail: List[Int]): Node[(Int, Kalaha)] =
      if !curr.hasKids then curr
      else tail match
          case Nil => curr
          case h::t => iterate(curr.getKids(h), t)
    iterate(root, path)
  

}

