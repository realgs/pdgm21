// Monika Jung L6
//
// W kazdym zadaniu (1-3) przygotowalam dwie wesje metod - wykorzystujace listy leniwe i uzywajace strumieni
//
// Glowna zaleta ewaluacji leniwej jest fakt, ze nieuzywane wartosci nie sa wyliczane i nie zajmuja pamieci
// a pamietane jest jedynie wyrazenie, za pomoca ktorego mozna te wartosci wyliczyc
// Ponadto taki sposob przechowywania danych o listach elementow, pozwala na tworzenie list nieskonczonych,
// co jest przydatne w wielu przypadkach.
// Podstawowa roznica miedzy listami leniwymi i strumieniami w Scali to sposob ich ewaluacji - mianowicie listy leniwe
// w calosci sa ewaluowane leniwie, natomiast w przypadku strumieni ich pierwszy element jest ewaluowany gorliwie.

import scala.annotation.tailrec

object l6comment {
  // Task 1 --------------------------------------------------------------------------------------------------------------

  // metoda wypisujaca n-te elementy strumienia poczawszy od pierwszego do wybranego ostatiego elementu (lastIndex)
  def eachNthElemS[A] (stream:Stream[A], eachN:Int, lastIndex:Int):Stream[A] =
    if lastIndex < 0 then throw new Exception("last index can't be smaller than 0!")
    else if eachN <= 0 then throw new Exception("N can't be smaller than 1!")
    else
      // metoda dopasowujaca ogon listy, aktualny indeks i graniczny indeks do wzorca
      @tailrec
      def recEachNthElem[A] (streamT:Stream[A], current:Int,  result:Stream[A]):Stream[A] =
        (streamT, current, lastIndex) match
          case (Stream(), _, _)                     => result
          case (_,_,0)                              => result
          case (head #:: tail, current, lastIndex)  => if current >= lastIndex then result
          else if current == 0 || current%eachN == 0
          then recEachNthElem(tail, current + 1, result#:::Stream(head))
          else recEachNthElem(tail, current + 1, result)
      recEachNthElem(stream,0, Stream())

  // metoda wypisujaca n-te elementy listy leniwej poczawszy od pierwszego do wybranego ostatiego elementu (lastIndex)
  def eachNthElem[A] (llist:LazyList[A], eachN:Int, lastIndex:Int):LazyList[A] =
    if lastIndex < 0 then throw new Exception("last index can't be smaller than 0!")
    else if eachN <= 0 then throw new Exception("N can't be smaller than 1!")
    else
      @tailrec
      def recEachNthElem[A] (llistT:LazyList[A], current:Int,  result:LazyList[A]):LazyList[A] =
        (llistT, current, lastIndex) match
          case (LazyList(), _, _)                   => result
          case (_,_,0)                              => result
          case (head #:: tail, current, lastIndex)  => if current >= lastIndex then result
          else if current == 0 || current%eachN == 0
          then recEachNthElem(tail, current + 1, result#:::LazyList(head))
          else recEachNthElem(tail, current + 1, result)
      recEachNthElem(llist,0, LazyList())

  // metoda wybierajace n-ty element listy poczynajac od zerowego, zwracajaca ten element i reszte listy (ogon listy)
  // (uzyteczna do metody  alternatywnej)
  @tailrec
  def takeNthElemOfLazyList[A](llist:LazyList[A], index:Int):(A,LazyList[A]) =
    if index < 0 then throw new Exception("wrong index!")
    else if index == 0 then (llist.head, llist.tail)
    else takeNthElemOfLazyList(llist.tail, index-1)

  // metoda alternatywna - zwracajaca od razu liste ewaluowana gorliwie n-tych elementow przeslanej listy leniwej
  // od jej pierwszego do ostatniego wybranego elementu
  def eachNthElemAternative[A](llist:LazyList[A], eachN:Int, lastIndex:Int):List[A] =
    if lastIndex < 0 then throw new Exception("last index can't be smaller than 0!")
    else if eachN <= 0 then throw new Exception("N can't be smaller than 1!")
    else
      @tailrec
      def recEachNthElem[A] (llistT:LazyList[A], current:Int, result:List[A]):List[A] =
      //println("start: " +result)
        if current + eachN > lastIndex then {
          //println("current: "+current+" >= last: " + lastIndex)
          result
        }
        else if current == 0 then recEachNthElem(llistT.tail, current+1, List(llistT.head))
        else {
          var pair: (A, LazyList[A]) = takeNthElemOfLazyList(llistT, eachN-1)
          //println("current: " + current + " < last: " + lastIndex)
          //println("pair: " + pair._1)
          //println(result)
          recEachNthElem(pair._2, current + eachN, result ::: List(pair._1))
        }
      recEachNthElem(llist,0, Nil)

  // Task 2 --------------------------------------------------------------------------------------------------------------
  // metoda dodajaca do siebie elementy dwoch list leniwych
  def addLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recAddLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recAddLazyListsElem(tailA,tailB,result#:::LazyList(headA+headB))
    recAddLazyListsElem(llistA,llistB,LazyList())

  // metoda odejmujaca od siebie elementy dwoch list leniwych
  def subtractLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recSubtractLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recSubtractLazyListsElem(tailA,tailB,result#:::LazyList(headA-headB))
    recSubtractLazyListsElem(llistA,llistB,LazyList())

  // metoda mnozaca przez siebie elementy dwoch list leniwych
  def multiplyLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recMultiplyLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recMultiplyLazyListsElem(tailA,tailB,result#:::LazyList(headA*headB))
    recMultiplyLazyListsElem(llistA,llistB,LazyList())

  // metoda dzielaca przez siebie elementy dwoch list leniwych
  def divideLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recDivideLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recDivideLazyListsElem(tailA,tailB,result#:::LazyList(headA/headB))
    recDivideLazyListsElem(llistA,llistB,LazyList())

  // metoda wywolujaca cztery podstawowe operacje matematyczne dla list leniwych
  def lazyExecute (llistA:LazyList[Int], llistB:LazyList[Int], sign:String):LazyList[Int] =
    sign match {
      case "+" => addLazyListsElements(llistA,llistB)
      case "-" => subtractLazyListsElements(llistA,llistB)
      case "*" => multiplyLazyListsElements(llistA,llistB)
      case "/" => divideLazyListsElements(llistA,llistB)
      case _ => throw new Exception("Wrong sign!!!")
    }

  // metoda dodajaca do siebie elementy dwoch strumieni
  def addStreamsElements (streamA:Stream[Int], streamB:Stream[Int]):Stream[Int] =
    @tailrec
    def recAddStreamElem (streamA:Stream[Int], streamB:Stream[Int], result:Stream[Int]):Stream[Int] =
      (streamA,streamB) match
        case (Stream(),_) => result#:::streamB
        case (_,Stream()) => result#:::streamA
        case (headA#::tailA, headB#::tailB) => recAddStreamElem(tailA,tailB,result#:::Stream(headA+headB))
    recAddStreamElem(streamA,streamB,Stream())

  // metoda odejmujaca od siebie elementy dwoch strumieni
  def subtractStreamsElements (streamA:Stream[Int], streamB:Stream[Int]):Stream[Int] =
    @tailrec
    def recSubtractStreamElem (streamA:Stream[Int], streamB:Stream[Int], result:Stream[Int]):Stream[Int] =
      (streamA,streamB) match
        case (Stream(),_) => result#:::streamB
        case (_,Stream()) => result#:::streamA
        case (headA#::tailA, headB#::tailB) => recSubtractStreamElem(tailA,tailB,result#:::Stream(headA-headB))
    recSubtractStreamElem(streamA,streamB,Stream())

  // metoda mnozaca przez siebie elementy dwoch strumieni
  def multiplyStreamsElements (streamA:Stream[Int], streamB:Stream[Int]):Stream[Int] =
    @tailrec
    def recMultiplyStreamElem (streamA:Stream[Int], streamB:Stream[Int], result:Stream[Int]):Stream[Int] =
      (streamA,streamB) match
        case (Stream(),_) => result#:::streamB
        case (_,Stream()) => result#:::streamA
        case (headA#::tailA, headB#::tailB) => recMultiplyStreamElem(tailA,tailB,result#:::Stream(headA*headB))
    recMultiplyStreamElem(streamA,streamB,Stream())

  // metoda dzielaca przez siebie elementy dwoch strumieni
  def divideStreamsElements (streamA:Stream[Int], streamB:Stream[Int]):Stream[Int] =
    @tailrec
    def recDivideStreamElem (streamA:Stream[Int], streamB:Stream[Int], result:Stream[Int]):Stream[Int] =
      (streamA,streamB) match
        case (Stream(),_) => result#:::streamB
        case (_,Stream()) => result#:::streamA
        case (headA#::tailA, headB#::tailB) => recDivideStreamElem(tailA,tailB,result#:::Stream(headA/headB))
    recDivideStreamElem(streamA,streamB,Stream())

  // metoda wywolujaca cztery podstawowe operacje matematyczne dla strumieni
  def lazyExecuteStream (streamA:Stream[Int], streamB:Stream[Int] sign:String):LazyList[Int] =
    sign match {
      case "+" => addStreamsElements(streamA,streamB)
      case "-" => subtractStreamsElements(streamA,streamB)
      case "*" => multiplyStreamsElements(streamA,streamB)
      case "/" => divideStreamsElements(streamA,streamB)
      case _ => throw new Exception("Wrong sign!!!")
    }

  // Task 3 --------------------------------------------------------------------------------------------------------------
  // metoda zwracajaca skonczona liste leniwa zawieracjaca okreslona ilosc argumentow o danej wartosci
  @tailrec
  def copyOneLazyListElem[A] (result:LazyList[A], elem:A, copyNr:Int):LazyList[A] =
    if copyNr <= 0 then result
    else copyOneLazyListElem(result #:::LazyList(elem), elem, copyNr - 1)

  // metoda zwracajaca liste leniwa zawieracjaca tyle kopii kadego argumentu listy wyjsciowej, ile okresla druga
  // przekazana do niej lista leniwa typu Int
  def copyLazyListElem[A] (llistA:LazyList[A], llistB:LazyList[Int]):LazyList[A] =
    @tailrec
    def recCopyLazyListElem[A] (llistA:LazyList[A], llistB:LazyList[Int], result:LazyList[A]):LazyList[A] =
      (llistA,llistB) match
        case (LazyList(),_) => result
        case (_,LazyList()) => result
        case (headA#::tailA, headB#::tailB) =>
          recCopyLazyListElem(tailA,tailB,copyOneLazyListElem(result,headA,headB))
    recCopyLazyListElem(llistA,llistB,LazyList())

  // metoda zwracajaca skonczona strumien zawieracjacy okreslona ilosc argumentow o danej wartosci
  @tailrec
  def copyOneStreamElem[A] (result:Stream[A], elem:A, copyNr:Int):Stream[A] =
    if copyNr <= 0 then result
    else copyOneStreamElem(result #:::Stream(elem), elem, copyNr - 1)

  // metoda zwracajaca strumien zawieracjacy tyle kopii kadego argumentu strumienia wyjsciowego, ile okresla drugi
  // przekazany do niej strumien typu Int
  def copyStreamElem[A] (streamA:Stream[A], streamB:Stream[Int]):Stream[A] =
    @tailrec
    def recCopyStreamElem[A] (streamA:Stream[A], streamB:Stream[Int], result:Stream[A]):Stream[A] =
      (streamA,streamB) match
        case (Stream(),_) => result
        case (_,Stream()) => result
        case (headA#::tailA, headB#::tailB) =>
          recCopyStreamElem(tailA,tailB,copyOneStreamElem(result,headA,headB))
    recCopyStreamElem(streamA,streamB,Stream())

  // Task 4 & 5 ----------------------------------------------------------------------------------------------------------

  trait Debug {
    def debugClassName(): String = getClass.getName()

    def debugClassVars(): List[List[Object]] =
      getClass.getDeclaredFields.toList.map {
        fun => fun.setAccessible(true)
          val result = List(fun.getName(), fun.getType(), fun.get(this))
          fun.setAccessible(false)
          result
      }
  }

  // klasa punkt
  class Point(xv: Int, yv: Int) extends Debug {
    // skladowe klasy
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]):Unit = {

    // tworze blizniacze listy leniwe i strumienie
    val println(llist1: LazyList[Int] = 1#::llist1.map(n=>n*2)
    //val llist1: LazyList[Int] = LazyList(<not computed>)
    val stream1: Stream[Int] = 1#::stream1.map(n=>n*2)
    //val stream1: Stream[Int] = Stream(1, <not computed>)
    val llist2: LazyList[Int] = 1#::llist2.map(n=>n+1)
    //val llist1: LazyList[Int] = LazyList(<not computed>)
    val stream2: Stream[Int] = 1#::stream2.map(n=>n+1)
    //val stream1: Stream[Int] = Stream(1, <not computed>)

    // TASK 1 ----------------------------------------------------------------------------------------------------------
    // wybieram pierwsze 6 elementow kazdej listy i strumienia (zeby zobaczyc, na czym bedziemy pracowac)
    println(llist1.take(6).toList == List(1, 2, 4, 8, 16, 32))
    println(stream1.take(6).toList == List(1, 2, 4, 8, 16, 32))
    println(llist2.take(6).toList == List(1, 2, 3, 4, 5, 6))
    println(stream2.take(6).toList == List(1, 2, 3, 4, 5, 6))

    //sprawdzenie poprawnosci metody wybrania n-tego elementu listy leniwej
    println(takeNthElemOfLazyList(llist1,1)._1 == 2)
    println(takeNthElemOfLazyList(llist1,2)._1 == 4)
    println(takeNthElemOfLazyList(llist1,3)._1 == 8)
    println(takeNthElemOfLazyList(llist1,4)._1 == 16)

    // test metod wyszukiwania n-tych elementow list i strumieni
    println( eachNthElemS(stream1,1,10).toList == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
    println(eachNthElemS(stream1,3,7).toList == List(1, 8, 64))
    println(eachNthElemS(stream1,3,8).toList == List(1, 8, 64))
    println(eachNthElemS(stream1,3,10).toList == List(1, 8, 64, 512))

    println(eachNthElem(llist1, 1, 10).toList == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
    println(eachNthElem(llist1, 3, 7).toList == List(1, 8, 64))
    println(eachNthElem(llist1, 3, 8).toList == List(1, 8, 64))
    println(eachNthElem(llist1, 3, 10).toList == List(1, 8, 64, 512))

    println(eachNthElemAternative(llist1,1,10) == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
    println(eachNthElemAternative(llist1, 3, 7) == List(1, 8, 64))
    println(eachNthElemAternative(llist1, 3, 8) == List(1, 8, 64))
    println(eachNthElemAternative(llist1,3,10) == List(1, 8, 64, 512))

    // TASK 2 ----------------------------------------------------------------------------------------------------------

    // tworze blizniacze listy leniwe i strumienie
    val llist01 = LazyList(7,8,9,20,30)
    val llist02 = LazyList(3,2,1)
    val stream01 = Stream(7,8,9,20,30)
    val stream02 = Stream(3,2,1)

    //test wywolywania operacji matematycznych na listach leniwych
    println(lazyExecute(llist01,llist02, "+").toList == List(10, 10, 10, 20, 30))
    println(lazyExecute(llist01,llist02, "-").toList == List(4, 6, 8, 20, 30))
    println(lazyExecute(llist01,llist02, "*").toList == List(21, 16, 9, 20, 30))
    println(lazyExecute(llist01,llist02, "/").toList == List(2, 4, 9, 20, 30))

    //test wywolywania operacji matematycznych na strumieniach
    println(lazyExecuteStream(stream01,stream02, "+").toList == List(10, 10, 10, 20, 30))
    println(lazyExecuteStream(stream01,stream02, "-").toList == List(4, 6, 8, 20, 30))
    println(lazyExecuteStream(stream01,stream02, "*").toList == List(21, 16, 9, 20, 30))
    println(lazyExecuteStream(stream01,stream02, "/").toList == List(2, 4, 9, 20, 30))

    // TASK 3 ----------------------------------------------------------------------------------------------------------

    // tworze blizniacze listy leniwe i strumienie
    val llist001 = LazyList(0,1,2,3,4,5)
    val llist002 = LazyList(3,2,0,1,2)
    val stream001 = Stream(0,1,2,3,4,5)
    val stream002 = Stream(3,2,0,1,2)

    // testy metod kopiujacych dla list leniwych i strumieni
    println(copyLazyListElem(llist001,llist002).toList == List(0, 0, 0, 1, 1, 3, 4, 4))
    println(copyStreamElem(stream001,stream002).toList == List(0, 0, 0, 1, 1, 3, 4, 4))

    // TASK 4 & 5 ------------------------------------------------------------------------------------------------------

    println("TASK 4 & 5 TEST")
    var p: Point = new Point (3,4)

    println(p.debugClassName())
    //val res25: String = Point
    println(p.debugClassVars())
    //val res26: List[List[Object]] = List(List(x, int, 3), List(y, int, 4), List(a, class java.lang.String, test))

  }
}
