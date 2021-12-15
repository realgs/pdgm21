//W swoich rozwiązaniach użyłem LazyList ze względu na korzyści, które oferuje, mianowicie fakt bycia strukturą niemutowalną, 
//o potencjalnie nieskończonej długości, pozwalającą nam jednak na wykorzystanie dokładnie takiej ilości danych, 
//jaką potrzebujemy, reszta nie jest ewaluowana bez potrzeby, a doskonałym przykładem tego jest zadanie 1, 
//gdzie ewaluowanie wartości powyżej indeksu "m" jest bezsensowne.
//Przeciwko użyciu strumieni przemawia fakt, że zostały one oznaczone jako "depricated" i fakt, że ewaluowany w nich leniwie jest jedynie ogon,
//podczas gdy głowa pozostaje ewaluowana gorliwie, co może być w niektórych sytuacjach mylące i co z resztą doprowadziło do zastąpienia strumieni przez LazyList.

import java.lang.reflect.Field

object lab6 {
  //Zadanie 1
  def eachNElement[A](llist: LazyList[A], n: Int, m: Int): LazyList[A] =
    if m < 0 || n < 1 then throw new Exception("Wrong input!")
    else
      def eachNElementI[A](llist: LazyList[A], counter: Int): LazyList[A] =
        llist match
          case LazyList() => LazyList()
          case h #:: t =>
            if counter == m then LazyList()
            else if counter % n == 0 then h #:: eachNElementI(t, counter + 1)
            else eachNElementI(t, counter + 1)
      eachNElementI(llist, 0)

  //Zadanie 2
  def lazyExecute(llist1: LazyList[Int], llist2: LazyList[Int], operator: Char): LazyList[Int] =
    (llist1, llist2) match
      case (LazyList(), _) => llist2
      case (_, LazyList()) => llist1
      case (h1 #:: t1, h2 #:: t2) =>
        operator match
          case '+' => (h1 + h2) #:: lazyExecute(t1, t2, operator)
          case '-' => (h1 - h2) #:: lazyExecute(t1, t2, operator)
          case '*' => (h1 * h2) #:: lazyExecute(t1, t2, operator)
          case '/' => (h1 / h2) #:: lazyExecute(t1, t2, operator)
          case _ => throw new Exception("Wrong operator!")

  //Zadanie 3
  def givenTimes[A](repeatList: LazyList[A], timesList: LazyList[Int]): LazyList[A] =
    (repeatList, timesList) match
      case (LazyList(), _) => LazyList()
      case (_, LazyList()) => LazyList()
      case (_ #:: t1, 0 #:: t2) => givenTimes(t1, t2)
      case (h1 #:: t1, left #:: t2) => h1 #:: givenTimes(repeatList, (left - 1) #:: t2)

  //Zadanie 4, 5
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  trait Debug {
    def debugName(): String = getClass().getSimpleName();

    def debugVars(): List[List[Any]] =
      def debugVarsI(fields: List[Field]): List[List[Any]] =
        fields match
          case Nil => Nil
          case h :: t =>
            h.setAccessible(true)
            List(h.getName(), h.getType().getSimpleName(), h.get(this)) :: debugVarsI(t)
      debugVarsI(getClass().getDeclaredFields().toList)
  }

  def main(args: Array[String]): Unit = {
    //Zadanie 1
    println(eachNElement(LazyList(), 3, 15).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2, 3).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3, 15).toList)
    println()

    //Zadanie 2
    println(lazyExecute(LazyList(4, 5, 6), LazyList(1, 2), '+').toList)
    println(lazyExecute(LazyList(4, 5, 6), LazyList(1, 2), '-').toList)
    println(lazyExecute(LazyList(4, 5, 6), LazyList(1, 2), '*').toList)
    println(lazyExecute(LazyList(4, 5, 6), LazyList(1, 2, 3), '/').toList)
    println()

    //Zadanie 3
    println(givenTimes(LazyList(1, 2, 3), LazyList(1, 2, 3)).toList)
    println(givenTimes(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList)
    println()

    //Zadanie 4
    var p: Point = new Point(3, 4)
    println(p.debugName())
    println()

    //Zadanie 5
    println(p.debugVars())
  }
}
