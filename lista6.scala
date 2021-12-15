import scala.annotation.tailrec
object Lista6 {

  def myMap[A, B](list: List[A], f: A => B): List[B] =
    list match
      case List() => List()
      case head :: tail => f(head) :: myMap(tail, f)

  def myReverse[A](list: LazyList[A]): LazyList[A] =
    @tailrec
    def reverseIter[A](reversedList: LazyList[A], list: LazyList[A]): LazyList[A] =
      list match
        case LazyList() => reversedList
        case h #:: t => reverseIter(h #:: reversedList, t)

    reverseIter(LazyList(), list)

  /*
        Ewaluacja leniwa pozwala na zmniejszenie złożoności obliczeniowej i pamięciowej - wartości są ewaluowane dopiero, gdy jest to konieczne. Jednak znacznie utrudnia określenie tych złożoności.
      Pozwala tworzyć struktury danych jak np. nieskończone listy/drzewa. W związku z tym przydatne choćby w matematyce (wykonywanie pewnych złożonych obliczeń tylko, gdy jest to konieczne, np. macierze).
      Wykorzystywane w systemach Unix do zwiększenia wydajności - ładowane są tylko te strony z dysku, które są wymagane.
      Dzięki temu, pamięć dla pozostałych stron nie jest alokowana. Przydatne przy dużych ilościach danych, nieskończonych cyklach.
      Np. obliczenie liczby Fibonnacciego.

          W zadaniach 1 oraz 2 jako reprezentację danych wykorzystałem LazyList, a nie strumienie - nie mają w tym przypadku zalet
      względem LazyList. Klasa Stream od wersji 2.13 ma staus deprecated, jest przestarzała. Przede wszystkim, w przypadku
      Stream jedynie ogon jest ewaluowany leniwie (wartość jest obliczana dopiero wtedy, gdy jest potrzebna) -
      głowa jest zawsze ewaluowana gorliwie, tzn. jest zawsze obliczana (następuje ewaluacja wyrażenia i związanie zmiennej
      z obliczoną wartością). Przewagę Stream może uzyskać jedynie w skrajnych sytuacjach, gdy przykładowo zawsze chcemy, aby pierwszy
      element był ewaluowany gorliwie.
          LazyList jest cała ewaluowana leniwie - zarówno głowa, jak i ogon nie są ewaluowane, dopóki nie jest to konieczne. Zmniejsza
      to złożoność obliczeniową.
  */

  //Zadanie 1
  def eachNElement[A](list: LazyList[A], nthElem: Int, lastElem: Int): LazyList[A] =
    if (nthElem <= 0) || (lastElem <= 0) then throw new Exception("nthElem and lastElem must be greater then 0!")
    @tailrec
    def eachNElemIter[A](listHelper: LazyList[A], acc: LazyList[A], indexElem: Int): LazyList[A]  =
      listHelper match
        case LazyList() => acc
        case head#::tail =>
          if indexElem >= lastElem then acc
          else if indexElem % nthElem == 0 then eachNElemIter(tail, head#::acc, indexElem+1)
          else eachNElemIter(tail, acc, indexElem+1)
    myReverse(eachNElemIter(list, LazyList(), 0))

  //Zadanie 2
  def lazyExecute(fstList: LazyList[Int], sndList: LazyList[Int], operator: Char): LazyList[Int] =
    @tailrec
    def lazyExecuteIter(fstList: LazyList[Int], sndList: LazyList[Int], acc: LazyList[Int], operation: (Int, Int) => Int): LazyList[Int] =
      (fstList, sndList) match
        case (fstH#::fstT, sndH#::sndT) => lazyExecuteIter(fstT, sndT, (operation(fstH, sndH))#::acc, operation)
        case (LazyList(), sndH#::sndT) => lazyExecuteIter(LazyList(), sndT, sndH#::acc, operation)
        case (fstH#::fstT, LazyList()) => lazyExecuteIter(LazyList(), fstT, fstH#::acc, operation)
        case (LazyList(), LazyList()) => acc
    operator match
      case '+' =>  myReverse(lazyExecuteIter(fstList,sndList, LazyList(), (a, b) => a+b))
      case '-' =>  myReverse(lazyExecuteIter(fstList,sndList, LazyList(), (a, b) => a-b))
      case '*' =>  myReverse(lazyExecuteIter(fstList,sndList, LazyList(), (a, b) => a*b))
      case '/' =>  myReverse(lazyExecuteIter(fstList,sndList, LazyList(), (a, b) => if b!= 0 then a/b else throw new Exception("Division by 0!")))
      case _ => throw new Exception("Wrong operator!")

  //Zadanie 3
  /*
  Jako reprezentację kolekcji wykorzystałem LazyList. Część zalet zostało wymienionych przy okazji komentowania zadań 1 i 2.
  Jedna z list: itemsList lub quantityList może być dłuższa - użycie LazyList daje nam tą korzyść, że odpowiednie elementy dłuższej
  listy nie są wartościowane = mniej obliczeń. Podobny zysk uzyskujemy w przypadku, gdy np. pierwszym elementem quantityList będzie 0
  Dodatkowo chciałbym wspomnieć o tym, iż Listy leniwe w Scali wykorzystują mechanizm spamiętywania (ang. memoization),
  dzięki czemu raz obliczone elementy listy leniwej nie są ponownie obliczane.
    */
  def duplicate[A](itemsList: LazyList[A], quantityList: LazyList[Int]): LazyList[A] =
   @tailrec
   def duplicateIter[A](itemsList: LazyList[A], quantityList: LazyList[Int], acc: LazyList[A]): LazyList[A] =
     (itemsList, quantityList) match
        case (fstH#::fstT, sndH#::sndT) =>
          if sndH > 0 then duplicateIter(fstH#::fstT, (sndH-1)#::sndT, fstH#::acc)
          else duplicateIter(fstT, sndT, acc)
        case (_, _) => acc
   myReverse(duplicateIter(itemsList, quantityList, LazyList()))

  //Zadania 4, 5
  trait Debug {

    def debugName(): String = this.getClass().getSimpleName()

    def debugVars(): List[Any] =
      var list = this.getClass.getDeclaredFields().toList
      myMap(list, (f => {f.setAccessible(true); List(f.getName(), f.getType(), f.get(this))}))
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList)
    println(eachNElement(LazyList(0,1,2,3,4,5,6), 2, 7).toList)
    //println(eachNElement(LazyList(0,1,2,3,4,5,6), 2, -1).toList)
    println(eachNElement(LazyList(), 2, 1).toList)

    println(lazyExecute(LazyList(2,2,2,2,2,2), LazyList(1,1,1,1,2,1,1,1), '/').toList)
    //println(lazyExecute(LazyList(2,2,2,2,2,2), LazyList(1,1,1,1,0,1,1,1), '/').toList)

    println(duplicate(LazyList(1, 2, 3), LazyList(2, 3, 4, 5)).toList)
    println(duplicate(LazyList(1, 2, 3, 8, 7, 6), LazyList(2, 3, 4, 5)).toList)
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList)
    println(duplicate(LazyList(1, 2, 3), LazyList()).toList)
    println(duplicate(LazyList(), LazyList(0, 3, 1, 4)).toList)
    println(duplicate(LazyList(), LazyList()).toList)

    var p: Point = new Point(3, 4)
    println(p.debugName())
    val method = p.getClass.getDeclaredField("x")
    method.setAccessible(true) //reflected object should suppress checks for Java language access control when it is used.
    println(p.debugVars())
  }
}


