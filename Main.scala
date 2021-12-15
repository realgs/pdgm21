object Main {

  //-------------------------------Introduction-------------------------------+
  // LAZYLIST ------> głowa oraz ogoń są ewaluowane leniwe!                   |
  // STREAM (Depricated) ---------> tylko ogoń jest ewaluowane leniwe.        |
  //                                                                          |
  // *LENIWE ---> to znaczy że wyrażenie w których inicjalizatory             |
  // nie są obliczane dopóki nie zostanie użyta val.                          |
  //                                                                          |
  //Ze względu na to, że z wersji Scali 2.13 kolekcja Stream stała depricated,|
  // z tego powodu w zadaniach zamiast Stream korzystałem z kolekcji LazyList |
  // z wersji 2.13.                                                           |
  //--------------------------------------------------------------------------+


  //----------------------------------Czemu LazyList, a nie coś innego??????----------------------------------------+
  //                                                                                                                |
  // [+] >>>>> LazyList może mieć nieskonczoną długość                                                              |
  // [+] >>>>> immutable                                                                                            |
  // [+] >>>>> Elementy LazyList są "memoized"; to znaczy, że wartość każdego elementu jest obliczana tylko raz.    |
  // [+] >>>>> Zmniejsza złożoność czasową algorytmu poprzez odrzucenie obliczeń tymczasowych i warunkowych.        |
  //                                                                                                                |
  // [-] >>>>> Zmusza on runtime języka do wstrzymania oceny podwyrażeń do momentu,                                 |
  //            gdy jest to wymagane w końcowym wyniku poprzez tworzenie thunks (obiektów opóźnionych).             |
  //                                                                                                                |
  //----------------------------------------------------------------------------------------------------------------+
  //source = [
  // 'https://en.wikipedia.org/wiki/Lazy_evaluation',
  // 'https://www.scala-lang.org/api/2.13.x/scala/collection/immutable/LazyList.html',
  // ]



  def eachNElement[A](llist: LazyList[A], whichIndex: Int, lastIndex: Int): LazyList[A] =
    def eachNElementHelper(llist: LazyList[A], indexCount: Int, pivotIndex: Int): LazyList[A] =
      (llist, indexCount == whichIndex, pivotIndex) match
        case (_, _, 0) => LazyList()
        case (LazyList(), _, _) => LazyList()
        case (h #:: t, true, _) => h #:: eachNElementHelper(t, 1, pivotIndex - 1)
        case (h #:: t, false, _) => eachNElementHelper(t, indexCount + 1, pivotIndex - 1)

    if whichIndex > 0 && lastIndex > 0 then eachNElementHelper(llist, whichIndex, lastIndex)
    else LazyList()


  def lazyExecute(llist1: LazyList[Int], llist2: LazyList[Int], op: Char): LazyList[Int] =
    def operation(num1: Int, num2: Int, op: Char): Int =
      op match
        case '+' => num1 + num2
        case '-' => num1 - num2
        case '*' => num1 * num2
        case '/' => if num2 != 0 then num1 / num2 else num1
        case _ => num1

    def lazyExecuteHelper(llist1: LazyList[Int], llist2: LazyList[Int]): LazyList[Int] =
      (llist1, llist2) match
        case (h1 #:: t1, h2 #:: t2) => operation(h1, h2, op) #:: lazyExecuteHelper(t1, t2)
        case (_, LazyList()) => llist1
        case (LazyList(), _) => llist2

    lazyExecuteHelper(llist1, llist2)


  def duplicate[A](llist: LazyList[A], repeats: LazyList[Int]): LazyList[A] = {
    def duplicateInner(llist: LazyList[A], repeats: LazyList[Int]): LazyList[A] =
      (llist, repeats) match
        case (_ #:: t1, 0 #:: t2) => duplicateInner(t1, t2)
        case (h1 #:: _, h2 #:: t2) => h1 #:: duplicateInner(llist, (h2 - 1) #:: t2)
        case (_, _) => LazyList()
    duplicateInner(llist, repeats)
  }

  trait Debug {
    def debugName() =
      this.getClass().getName()

    def showVar(arg: java.lang.reflect.Field) =
      arg.setAccessible(true)
      (arg.getName(), arg.getType(), arg.get(this))

    def debugVars =
      this.getClass().getDeclaredFields().toList.map(arg => showVar(arg))
  }

  class Point(x: Int, y: Int) extends Debug {
    var this.x: Int = x
    var this.y: Int = y
    var str: String = "123"
  }

  def main(args: Array[String]): Unit = {
    println("EACHNELEMENT TESTS")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).take(10).toList)
    println(eachNElement(LazyList(), 2, 11111111).take(10).toList)
    println(eachNElement(LazyList.from(1), 2, 1111111).take(10).toList)

    println("LAZYEXECUTES TESTS")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(1, 2), '-').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(1, 1, 0), '*').take(10).toList)
    println(lazyExecute(LazyList(1, 9, 6, 196), LazyList(1, 3, 6), '/').take(10).toList)
    println(lazyExecute(LazyList(1, 9, 6, 196), LazyList(), '/').take(10).toList)

    println("LREPEAT TESTS")
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).take(10).toList)
    println(duplicate(LazyList.from(0), LazyList.from(0)).take(10).toList)
    println(duplicate(LazyList(), LazyList(0, 0, 0, 4)).take(10).toList)


    println("DEBUG NAME TESTS")
    var p1: Point = new Point(3, 4)
    var p2: Point = new Point(0, 0)
    var p3: Point = new Point(-1, -3)
    println(p1.debugName())
    println(p2.debugName())
    println(p3.debugName())

    println("DEBUG VARS TESTS")
    println(p1.debugVars)
    println(p2.debugVars)
    println(p3.debugVars)
  }
}