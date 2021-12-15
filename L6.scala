//                            LazyList       vs         Strumienie
//                               |                          |
//                               v                          v
//głowa wraz z ogonem ewoluowana leniwie     // głowa zachłannie !, ogon leniwie
//Do poniższych zadań uzyłem list leniwych- struktur niemutowalnych ze względu na:
//wartosci lazyListy są obliczane kolejno, tylko wtedy gdy są nam potrzebne,
//co najwyżej raz i nie są pomijane- wzrost wydajnosci, unikanie zbędnych obliczeń
//tworzenie nieskończonych struktur danych pozwala na poszerzenie możliwości operacji na nich
//W strumieniach głowa jest ewoluowana zachłannie, co w naszych zadaniach jest zbędne,
//strumienie są obecnie zastępowane Lazylist'ami

object L6 {
  //zadanie 1
  def eachNElement[A](lList: LazyList[A], nextElem: Int, end: Int): LazyList[A] =
    def helper(lList: LazyList[A], position: Int): LazyList[A] =
      if lList == LazyList() || position == end then LazyList()
      else if position % nextElem != 0 then helper(lList.tail, position + 1)
      else lList.head #:: helper(lList.tail, position + 1)
    if nextElem > 0 && end > 0 then helper(lList, 0) else LazyList()


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

  //zadanie 3
  def lrepeat[A](llist: LazyList[A], duplicator: LazyList[Int]): LazyList[A] =
    def helper(llistRepeated: LazyList[A], duplicatorHelper: LazyList[Int]): LazyList[A] =
      (llistRepeated, duplicatorHelper) match
        case (_, LazyList()) => LazyList()
        case (LazyList(), _) => LazyList()
        case (_ #:: t1, 0 #:: t2) => helper(t1, t2)
        case (h1 #:: _, h2 #:: t2) => h1 #:: helper(llistRepeated, (h2 - 1) #:: t2)
    helper(llist, duplicator)

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


  def main(args: Array[String]) : Unit ={

    //testy zadanie 1
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3))
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6), -1, 5).toList == List())
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6), -1, 5).toList == List())
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6), 5, -1).toList == List())
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6), 2, 6).toList == List(1, 3, 5))
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6), 5, 6).toList == List(1, 6))
    println(eachNElement(LazyList(), 5, 7).toList == List())
    println(eachNElement(LazyList(5, 6, 4, 3, 2, 1), 2, 3).take(2).toList == List(5, 4))

    //testy zadanie 2
    println(lazyExecute(LazyList(), LazyList(2, 3, 4, 5), '+').force == LazyList(2, 3, 4, 5))
    println(lazyExecute(LazyList(1, 2, 3), LazyList(), 's').force == LazyList(1, 2, 3))
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), 's').force == LazyList())
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), 's').force == LazyList())
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').force == LazyList(3, 5, 7, 5))
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').force == LazyList(-1, -1, -1, 5))
    println(lazyExecute(LazyList(10, 20, 30), LazyList(-2, -3, -4, -5), '+').force == LazyList(8, 17, 26, -5))
    println(lazyExecute(LazyList(10, 20, 30), LazyList(-2, -3, -4, -5), '-').force == LazyList(12, 23, 34, -5))
    println(lazyExecute(LazyList(10, 20, 30), LazyList(5, -2), '*').force == LazyList(50, -40, 30))
    println(lazyExecute(LazyList(10, 20, 30), LazyList(5, -2), '/').force == LazyList(2, -10, 30))

    //testy zadanie 3
    println(lrepeat(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).force == LazyList(2, 2, 2, 3))
    println(lrepeat(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4)).force == LazyList(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
    println(lrepeat(LazyList(), LazyList(1, 2, 3, 4)).force == LazyList())
    println(lrepeat(LazyList(1, 2, 3, 4), LazyList()).force == LazyList())
    println(lrepeat(LazyList(1, 2, 3, 4), LazyList(1, 6)).force == LazyList(1, 2, 2, 2, 2, 2, 2))

    var p : Point = new Point(3, 4);
    println(p.debugName())
    println(p.debugVars())

  }
}
