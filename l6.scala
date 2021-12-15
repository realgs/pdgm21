object l6 {

  // W zadaniach od 1 do 3 użyłem listy leniwej, ponieważ nie ma powodu żeby gorliwie ewaluować głowę (tak by było w przypadku użycia strumieni). Poza tym strumienie w Scali 3 są deprecated.
  // Użycie w poniższych struktur leniwych, które nie są w całości ewaluowane gorliwie jest uzasadnione, ponieważ nie zawsze ewaluacja całych kolekcji będzie wymagana.
  // Na przykład w zadaniu 1, po przekroczeniu indeksu otatniego przetwarzanego elementu, reszta listy jest ignorowana.

  def eachNElement[A](list: LazyList[A], interval: Int, lastIndex: Int): LazyList[A] =
    (list, lastIndex) match
      case (LazyList(), _) => list
      case (_, lastIndex) if lastIndex <= interval => LazyList.cons(list.head, LazyList())
      case (list, lastIndex) => LazyList.cons(list.head, eachNElement(list.splitAt(interval)._2, interval, lastIndex-interval))

  def +(a:Int, b: Int): Int = a + b
  def -(a:Int, b: Int): Int = a - b
  def *(a:Int, b: Int): Int = a * b
  def /(a:Int, b: Int): Int = if b==0 then {println("Cannot divide by 0"); a} else a / b

  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], oper: (Int, Int) => Int): LazyList[Int] =
    (list1, list2) match
      case (LazyList(), LazyList()) => list1
      case (list1, LazyList()) => list1
      case (LazyList(), list2) => list2
      case (list1, list2) => LazyList.cons(oper(list1.head, list2.head), lazyExecute(list1.tail, list2.tail, oper))

  def multiplyLists[A](originalList: List[A], multipliers: List[Int]): LazyList[A] =
    (originalList, multipliers) match
      case (Nil, _) => LazyList()
      case (_, Nil) => LazyList()
      case (originalListHD::originalListTL, multipliersHD::multipliersTL) => if multipliersHD==0 then multiplyLists(originalListTL, multipliersTL)
                                                                             else LazyList.cons(originalListHD, multiplyLists(originalList, (multipliersHD-1)::multipliersTL))

  trait Debug {
    def debugName(): String = this.getClass().getSimpleName()
    def debugVars(): List[Matchable] = this.getClass().getDeclaredFields().map(f => (f.getName, f.getType, {f.setAccessible(true); f.get(this)})).toList
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {
    var p : Point = new Point(3, 4)

    println(p.debugName())
    p.debugVars().foreach(println)

  }

}