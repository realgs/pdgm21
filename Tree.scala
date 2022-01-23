import scala.annotation.tailrec

def aToPowerb(a: Int, b:Int): Int =
  @tailrec
  def iterator(result: Int, rest: Int): Int =
    rest match
      case 1 => result
      case 0 => 1
      case _ => iterator(result*a, rest - 1)
  iterator(a, b)

class Node(var value: (Int, Kalaha, Int), val size: Int) {

  private val kids = new Array[Node](size)
  private var index = 0

  def getValue:(Int, Kalaha, Int) = value

  def setValue(newValue: (Int, Kalaha, Int)): Unit = value = newValue
  
  def hasKids: Boolean = index != 0

  def getKids: Array[Node] = kids

  def getIndex: Int = index

  def hasAllKids: Boolean = index == size
  
  def addKid(newKid: Node): Unit =
    kids(index) = newKid
    if index == -1 then index = 1
    else index += 1

  def addNewKid(value: (Int, Kalaha, Int)): Unit =
    kids(index) = new Node(value, kids.length)
    if index == -1 then index = 1
    else index += 1

  def addKidAt(newKid: Node, index: Int): Unit =
    kids(index) = newKid
    if this.index == 0 then this.index = -1

  def addNewKidAt(value: (Int, Kalaha, Int), index: Int): Unit =
    kids(index) = new Node(value, kids.length)
    if this.index == 0 then this.index = -1

  def findPaths(): Array[((Int, Kalaha, Int), List[Int])] =
    var res = new Array[((Int, Kalaha, Int), List[Int])](0)
    def search(curr: Node, path: List[Int]): Unit =
      if curr.hasKids then
        var i = 0
        while(i < size){
          res = res:+(curr.getKids(i).getValue, path:+i)
          search(curr.getKids(i), path:+i)
          i += 1
        }
    search(this,List())
    res

  def maxKid(): Int =
    var index = 0
    var i = 0
    var maxValue = kids(0).value._1
    while (i < kids.length){

      if kids(i).value._1 > maxValue then {
        index = i
        maxValue = kids(i).value._1
      }
      i += 1
    }
    index

  def minKid(): Int =
    var index = 0
    var i = 0
    var minValue = kids(0).value._1
    while (i < kids.length){

      if kids(i).value._1 < minValue then {
        index = i
        minValue = kids(i).value._1
      }
      i += 1

    }
    index

  def chooseBest(mine: Boolean): Int =
    if mine then maxKid()
    else minKid()
  
  def countNodes(): Int =
    @tailrec
    def count(stack: List[Node], res: Int): Int =
      stack match {
        case List() => res
        case h::t =>
          if h.hasKids then count(h.kids.toList:::t, res+1)
          else count(t, res+1)
      }
    count(List(this), 0)

  def fullHeight(): Int =
    val nodesAmount = countNodes()
    @tailrec
    def counter(rest: Int, power: Int): Int =
      if rest < 0 then power - 1
      else counter(rest - aToPowerb(size, power), power + 1)
    counter(nodesAmount,0)

  def bestChoice(playerId: Int): Array[(Int, Int)] =
    val res = Array.fill(6)((0, 0))
    def iterator(curr: Node, index: Int, depth: Int): Unit =
      if curr.hasKids && depth >= 0 then {
       res(index) = (curr.chooseBest(curr.value._3 == playerId), curr.value._3)
       curr.kids.foreach(kid => {
         iterator(kid, index, depth - 1)
       })
     }
    var i = 0
    val depth: Int = fullHeight()
    kids.foreach(kid => {
      iterator(kid, i, depth - 2)
      i += 1
    })
    res

  def countSubtree(): Unit =
    def kidCount(): Int =
      var res = 0
      if hasAllKids then
        kids.foreach(kid => {
          kid.countSubtree()
          res = res + kid.value._1
        })
      res / kids.length
    value = (value._1 + kidCount(), value._2, value._3)

  def print(): Unit = printf("%d",value._1)
}

class MyTree(val kidsLimit: Int, game: Kalaha){

  var root: Node = Node((0, game, 0), kidsLimit)

  def getRoot: Node = root
  
  def setRoot(newRoot: Node): Unit = root = newRoot
  
  def getNode(path: List[Int]): Node =
    @tailrec
    def iterate(curr: Node, tail: List[Int]): Node =
      if !curr.hasKids then curr
      else tail match
        case Nil => curr
        case h::t => iterate(curr.getKids(h), t)
    iterate(root, path)

  def print(): Unit =
    @tailrec
    def iterator(queue: List[Node]): Unit ={
      queue match {
        case List() => ()
        case h::t =>
          printf("%d ",h.getValue._1)
          iterator(t:::h.getKids.toList)
      }
    }
    iterator(List(root))
}

