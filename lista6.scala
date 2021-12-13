import scala.annotation.tailrec

object lista6 {

  def revList[A](list: List[A]): List[A] =
      @tailrec
        def revListHelper(list: List[A], result: List[A]): List[A] =
            if list == Nil then result
            else revListHelper(list.tail, list.head :: result)
        revListHelper(list, List())


  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
    @tailrec
      def eachNElementTail[A](list: LazyList[A], k: Int, m: Int, finalList: LazyList[A]): LazyList[A] =
        (list, m) match
          case (_, 0) => finalList
          case (LazyList(), _) => finalList
          case (h#::t, m) => if k == n then eachNElementTail(t, 1, m-1, h#::finalList)
                             else eachNElementTail(t, k+1, m-1, finalList)
      eachNElementTail(list, n, m, LazyList())


  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], symbol: Char): LazyList[Int] =
    @tailrec
      def lazyExecuteTail(list1: LazyList[Int], list2: LazyList[Int], symbol: Char, finalList: LazyList[Int]): LazyList[Int] =
        (list1, list2, symbol) match
          case (LazyList(),LazyList(),   _  ) => finalList
          case (LazyList(),h2#::t2   ,symbol) => lazyExecuteTail(list1,t2    ,symbol,h2#::finalList     )
          case (h1#::t1   ,LazyList(),symbol) => lazyExecuteTail(t1   ,list2 ,symbol,h1#::finalList     )
          case (h1#::t1   ,h2#::t2   ,  '+' ) => lazyExecuteTail(t1   ,t2    ,symbol,(h1+h2)#::finalList)
          case (h1#::t1   ,h2#::t2   ,  '-' ) => lazyExecuteTail(t1   ,t2    ,symbol,(h1-h2)#::finalList)
          case (h1#::t1   ,h2#::t2   ,  '*' ) => lazyExecuteTail(t1   ,t2    ,symbol,(h1*h2)#::finalList)
          case (h1#::t1   ,h2#::t2   ,  '/' ) => lazyExecuteTail(t1   ,t2    ,symbol,(h1/h2)#::finalList)
      lazyExecuteTail(list1, list2, symbol, LazyList())


  def repeatNTimes[A](list: LazyList[A], listOfN: LazyList[Int]): List[A] =
    @tailrec
      def repeatNTimesTail[A](list: LazyList[A], listOfN: LazyList[Int], k: Int, ansList: List[A]): List[A] =
        (list, listOfN) match
          case (LazyList(), _) => ansList
          case (_, LazyList()) => ansList
          case (h#::t, hN#::tN) => if k == hN then repeatNTimesTail(t, tN, 0, ansList)
                                   else repeatNTimesTail(list, listOfN, k+1, h::ansList)
      repeatNTimesTail(list, listOfN, 0, Nil)


  trait Debug {
    def debugName(): String =
        getClass.getSimpleName

    def debugVars(): List[Any] =
        getClass.getDeclaredFields.toList.map(field =>
          field.setAccessible(true)
          (field.getName, field.getType, field.get(this)))
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit =

    // Zadanie 1
    println("Zadanie 1:")
    println( revList( eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList ))
    println( revList( eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList ))

    // Zadanie 2
    println("Zadanie 2:")
    println(revList(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList))
    println(revList(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList))
    println(revList(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList))
    println(revList(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '/').toList))
    println(revList(lazyExecute(LazyList(2, 3, 4, 5), LazyList(1, 2, 3), '+').toList))

    // Zadanie 3
    println("Zadanie 3:")
    println(revList(repeatNTimes(LazyList(1,2,3), LazyList(0,3,1,4))))
    println(revList(repeatNTimes(LazyList(1,2,3,4,5), LazyList(1,2))))

    // Zadanie 4 + 5
    println("Zadanie 4 + 5:")
    var p : Point = new Point(3,4)
    println(p.debugName())
    println(p.debugVars())
}

