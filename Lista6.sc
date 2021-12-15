//zadanie 1
def eachNElement[A](lList: LazyList[A], nextElem: Int, end: Int): LazyList[A] =
  def helper(lList: LazyList[A], position: Int): LazyList[A] =
    if lList == LazyList() || position == end then LazyList()
    else if position % nextElem != 0 then helper(lList.tail, position + 1)
    else lList.head #:: helper(lList.tail, position + 1)
  if nextElem > 0 && end > 0 then helper(lList, 0) else LazyList()


eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3)
eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3)
eachNElement(LazyList(1, 2, 3, 4, 5, 6), -1, 5).toList == List()
eachNElement(LazyList(1, 2, 3, 4, 5, 6), -1, 5).toList == List()
eachNElement(LazyList(1, 2, 3, 4, 5, 6), 5, -1).toList == List()
eachNElement(LazyList(1, 2, 3, 4, 5, 6), 2, 6).toList == List(1, 3, 5)
eachNElement(LazyList(1, 2, 3, 4, 5, 6), 5, 6).toList == List(1, 6)
eachNElement(LazyList(), 5, 7).toList == List()
eachNElement(LazyList(5, 6, 4, 3, 2, 1), 2, 3).take(2).toList == List(5, 4)

//zadanie 2
def lazyExecute(llist1: LazyList[Int], llist2: LazyList[Int], operator: Char): LazyList[Int] =
  (llist1, llist2) match
    case (_, LazyList()) => llist1
    case (LazyList(), _) => llist2
    case (h1 #:: t1, h2#::t2) => operator match
      case '+' => (h1 + h2) #:: lazyExecute(t1, t2, operator)
      case '-' => (h1 - h2) #:: lazyExecute(t1, t2, operator)
      case '*' => (h1 * h2) #:: lazyExecute(t1, t2, operator)
      case '/' => (h1 / h2) #:: lazyExecute(t1, t2, operator)
      case _ => LazyList()

lazyExecute(LazyList(), LazyList(2, 3, 4, 5), '+').force == LazyList(2, 3, 4, 5)
lazyExecute(LazyList(1, 2, 3), LazyList(), 's').force == LazyList(1, 2, 3)
lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), 's').force == LazyList()
lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), 's').force == LazyList()
lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').force == LazyList(3, 5, 7, 5)
lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').force == LazyList(-1, -1, -1, 5)
lazyExecute(LazyList(10, 20, 30), LazyList(-2, -3, -4, -5), '+').force == LazyList(8, 17, 26, -5)
lazyExecute(LazyList(10, 20, 30), LazyList(-2, -3, -4, -5), '-').force == LazyList(12, 23, 34, -5)
lazyExecute(LazyList(10, 20, 30), LazyList(5, -2), '*').force == LazyList(50, -40, 30)
lazyExecute(LazyList(10, 20, 30), LazyList(5, -2), '/').force == LazyList(2, -10, 30)

//zadanie 3
def lrepeat[A](llist: LazyList[A], duplicator: LazyList[Int]): LazyList[A] =
  def helper(llistRepeated: LazyList[A], duplicatorHelper: LazyList[Int]): LazyList[A] =
    (llistRepeated, duplicatorHelper) match
      case (_, LazyList()) => LazyList()
      case (LazyList(), _) => LazyList()
      case (_ #:: t1, 0 #:: t2) => helper(t1, t2)
      case (h1 #:: _, h2 #:: t2) => h1 #:: helper(llistRepeated, (h2 - 1) #:: t2)
  helper(llist, duplicator)

lrepeat(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).force == LazyList(2, 2, 2, 3)
lrepeat(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4)).force == LazyList(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
lrepeat(LazyList(), LazyList(1, 2, 3, 4)).force == LazyList()
lrepeat(LazyList(1, 2, 3, 4), LazyList()).force == LazyList()
lrepeat(LazyList(1, 2, 3, 4), LazyList(1, 6)).force == LazyList(1, 2, 2, 2, 2, 2, 2)

//zadanie 4 i 5
trait Debug {
  def debugName(): String = getClass.getSimpleName

  def debugVars(): List[Any] =
    getClass.getDeclaredFields.toList.map(fields => {
      fields.setAccessible(true)
      (fields.getName, fields.getType, fields.get(this))
    })
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3, 4);
p.debugName();
p.debugVars();
