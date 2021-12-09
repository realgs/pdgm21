//zadanie 1
def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
  def eachNElementTail[A](list: LazyList[A], k: Int, m: Int, finalList: LazyList[A]): LazyList[A] =
    (list, m) match
      case (_, 0) => finalList
      case (LazyList(), _) => finalList
      case (h#::t, m) => if k == n then eachNElementTail(t, 1, m-1, h#::finalList)
                            else eachNElementTail(t, k+1, m-1, finalList)

  eachNElementTail(list, n, m, LazyList()).reverse

eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList
eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList



//zadanie 2
def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], symbol: Char): LazyList[Int] =
  def lazyExecuteTail(list1: LazyList[Int], list2: LazyList[Int], symbol: Char, finalList: LazyList[Int]): LazyList[Int] =
    (list1, list2, symbol) match
      case (LazyList(), LazyList(), _) => finalList
      case (LazyList(), h#::t, symbol) => lazyExecuteTail(list1, t, symbol, h#::finalList)
      case (h#::t, LazyList(), symbol) => lazyExecuteTail(t, list2, symbol, h#::finalList)
      case (h1#::t1, h2#::t2, '+') => lazyExecuteTail(t1, t2, symbol, (h1+h2)#::finalList)
      case (h1#::t1, h2#::t2, '-') => lazyExecuteTail(t1, t2, symbol, (h1-h2)#::finalList)
      case (h1#::t1, h2#::t2, '*') => lazyExecuteTail(t1, t2, symbol, (h1*h2)#::finalList)
      case (h1#::t1, h2#::t2, '/') => lazyExecuteTail(t1, t2, symbol, (h1/h2)#::finalList)

  lazyExecuteTail(list1, list2, symbol, LazyList()).reverse

lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList
lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList
lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList
lazyExecute(LazyList(10, 20, 30), LazyList(2, 3, 4, 5), '/').toList
lazyExecute(LazyList(2, 3, 4, 5), LazyList(1, 2, 3), '+').toList



//zadanie 3
def repeatN[A](list: LazyList[A], listOfN: LazyList[Int]): List[A] =
  def repeatNTail[A](list: LazyList[A], listOfN: LazyList[Int], k: Int, finalList: List[A]): List[A] =
    (list, listOfN) match
      case (LazyList(), _) => finalList
      case (_, LazyList()) => finalList
      case (h#::t, n#::nTail) => if k >= n then repeatNTail(t, nTail, 0, finalList)
                                    else repeatNTail(list, listOfN, k+1, h::finalList)

  repeatNTail(list, listOfN, 0, Nil).reverse

repeatN(LazyList(1,2,3), LazyList(0,3,1,4,5,6))
repeatN(LazyList(1,2,3,4,5,6,7), LazyList(2,3))



//zadania 4 i 5
trait Debug {
  def debugName(): String =
    getClass.getName

  def getList(fields: List[java.lang.reflect.Field], finalList: List[List[Any]]): List[List[Any]] =
    fields match
      case Nil => finalList.reverse
      case h::t => {h.setAccessible(true)
                    getList(t, List(h.getName, h.getType, h.get(this))::finalList)}

  def debugVars(): List[List[Any]] =
    getList(getClass.getDeclaredFields.toList, Nil)
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3, 4)
p.debugName()
p.debugVars()
