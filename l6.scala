import scala.annotation.tailrec
import java.lang.reflect.Field

object l6 {

  //Funkcje dodatkowe

  def revLlist[A](lazyList: LazyList[A]):LazyList[A]={
    @tailrec
    def revTail[A](lList: LazyList[A], listOut: LazyList[A]):LazyList[A]={
      lList match
        case LazyList() => listOut
        case head#::tail => revTail(tail, head#::listOut)
    }
    revTail(lazyList, LazyList())
  }

  /*
Termin oddania listy: 15 / 16 grudnia 2021 w zależności od grupy zajęciowej.
Łączna liczba punktów: 20
  */

  /*

1) Zdefiniuj funkcję "eachNElement" wybierającą co n-ty element listy leniwej,
   zaczynając od elementu pierwszego kończąc na elemencie numer m(indeks m dotyczy listy pierwotnej, nie wynikowej).
   Czy w zadaniu warto użyć strumieni jako reprezentację danych? Jakie są ich zalety w tym przypadku?

   Przykłady:
   [5;6;3;2;1], 2, 3 -> [5;3]
   [5;6;3;2;1], 2, 4 -> [5;3]
   Wyniki powinny być zapisane w postaci leniwej

   Punkty: 4

*/

  def eachNElement[A](llist: LazyList[A], n: Int, endIndex: Int):LazyList[A]={
    def eachNElementTail[A](llist: LazyList[A], currentIndex:Int, outList: LazyList[A]):LazyList[A]=
      (llist, currentIndex) match
        case (LazyList(), _) => outList
        case (headLlist#::tailLlist, _)=>
          if currentIndex>=endIndex then outList
          else
            if currentIndex % n == 0 then
              eachNElementTail(tailLlist, currentIndex+1, headLlist#::outList)
            else
              eachNElementTail(tailLlist, currentIndex+1, outList)
    revLlist(eachNElementTail(llist, 0, LazyList()))
  }



/*
2) Zdefiniuj funkcję "lazyExecute" przyjmującą dwie listy leniwe i wykonującą podane działanie na elementach list. Obsłuż 4 podstawowe operacje matematyczne. Wynikiem jest lista leniwa.
   Czy w zadaniu warto użyć strumieni jako reprezentację danych? Jakie są ich zalety w tym przypadku?

   Przykład:
   [1;2;3], [2;3;4;5] oraz + daje [3;5;7;5]
   Wyniki powinny być zapisane w postaci leniwej

   Punkty: 4
*/

  def lazyExecute(firstLList: LazyList[Int], secondLList: LazyList[Int]):LazyList[Int]=
    (firstLList, secondLList) match
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), _) => secondLList
      case (_, LazyList()) => firstLList
      case (headFirst#::tailFirst, headsecond#::tailSecond) => (headFirst+headsecond)#::lazyExecute(tailFirst, tailSecond)




/*
3) Napisz funkcję powielającą elementy w kolekcji na podstawie drugiej kolekcji
   określającej ile razy elementy mają być powielone. Użyj dogodnej reprezentacji kolekcji i uzasadnij swój wybór. Tablice nie są optymalnym wyborem, można je potraktować jako absolutne minimum.

   przykład: wywołanie duplicate dla [1;2;3] oraz [0;3;1;4] daje wynik [2;2;2;3]

   Punkty: 4
*/

def replicateElements[A](elementsLList: LazyList[A], countsLList: LazyList[Int]):LazyList[A]={
  def innerReplicate[A](element: A, v: Int):LazyList[A]=
    (element, v) match
      case (_, 0) => LazyList()
      case (_,_) => element #:: innerReplicate(element, v-1)
  (elementsLList, countsLList) match
    case (_, LazyList()) => LazyList()
    case (LazyList(), _)  => LazyList()
    case (headElements#::tailElements, headCounts#::tailCounts) =>
      innerReplicate(headElements, headCounts)#:::replicateElements(tailElements, tailCounts)
}


/*
Lista metod potencjalnie przydatnych do poniższych zadań:

- getClass
- getDeclaredFields
- setAccessible
- getName
- get
*/

/*
4) Napisz cechę (trait) "Debug" z metodą "debugName", która zwraca nazwę klasy,
   do której cecha została dołączona.

Przykładowo:

class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
}

var p : Point = new Point(3, 4);
p.debugName();

Zwróci: Point

Punkty: 2
*/

  trait Debug {
    def debugName() : String = getClass.getSimpleName
    def debugVars() = {
      def debugVarsInner(declaredFieldsArray: List[Field]):List[List[String]]=
        declaredFieldsArray match
          case List() => List()
          case field::remainFields =>
            field.setAccessible(true)
            List(field.getName, field.getType.getName, field.get(this).toString) :: debugVarsInner(remainFields)
      val declaredFields = getClass.getDeclaredFields
      debugVarsInner(declaredFields.toList)
    }

  }

/*
5) Do trait dopisz metodę "debugVars", która zwraca
   listę nazw, typów pól, wartości pól, do której cecha została dołączona.

var p : Point = new Point(3, 4);
p.debugVars();

Zwróci:

[[x, int, 3], [y, int, 4], [a, java.lang.String, test]]

Punkty: 6
  */


  def main(args: Array[String]): Unit = {
    println("\nZadanie 1")
    println(eachNElement(LazyList(5,6,3,2,1), 2, 3).toList)
    println(eachNElement(LazyList(5,6,3,2,1), 2, 4).toList)
    println(eachNElement(LazyList.from(1), 3, 20).toList)

    println("\nZadanie 2")
    println(lazyExecute(LazyList.from(1).take(5), LazyList.from(1).take(6)).toList)
    println(lazyExecute(LazyList.from(1).take(10), LazyList()).toList)

    println("\nZadanie 3")
    println(replicateElements(LazyList.from(1).take(4), LazyList.from(1).take(4)).toList)
    println(replicateElements(LazyList.from(1).take(10), LazyList()).toList)
    println(replicateElements(LazyList(), LazyList.from(1).take(10)).toList)
    println(replicateElements(LazyList.from(1).take(5), LazyList.from(1).take(2)).toList)

    println("\nZadanie 4")

    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "test"
    }

    val p : Point = new Point(3, 4)
    println(p.debugName())

    println("\nZadanie 5")
    println(p.debugVars())


  }

}
