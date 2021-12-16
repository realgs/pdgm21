import math.Numeric.Implicits.infixNumericOps

object Main extends App {
  private lazy val lList1: LazyList[Int] = 1 #:: 2 #:: lList1.tail.map(n => n + 1)
  private lazy val lList2: LazyList[Int] = 2 #:: 4 #:: lList2.tail.map(n => n + 2)

  //ZADANIE 1
  def eachNElement[A](lList: LazyList[A], n: Int, end: Int): LazyList[A] =
    def eachNElementIter(lList: LazyList[A], end: Int, toSkip: Int): LazyList[A] =
      if (end == 0 || lList.isEmpty) then LazyList.empty
      else if toSkip != 0 then eachNElementIter(lList.tail, end - 1, toSkip - 1)
      else
        lList.head #:: eachNElementIter(lList, end - 1, n)

    if (n <= 0 || end <= 0 || lList.isEmpty) then LazyList.empty
    else
      lList.head #:: eachNElementIter(lList.tail, end - 1, n - 1)

  //TEST - ZADANIE 1
  println("***ZADANIE 1 - TEST***")
  println("Wyswietlam 7 elementow listy: ")
  lList1.take(7).foreach(println)
  println("Wyswietlam elementy wywolania eachNElement(lList, 3, 7)")
  lazy val resultlList = eachNElement(lList1, 3, 7)
  resultlList.take(12).foreach(println)

  //ZADANIE 2
  def lazyExecute[A: Numeric](lList1: LazyList[A], lList2: LazyList[A], operation: String): LazyList[A] =
    (lList1, lList2) match
      case (h1 #:: t1, h2 #:: t2) =>
        operation match
          case "+" => (h1 + h2) #:: lazyExecute(t1, t2, operation)
          case "-" => (h1 - h2) #:: lazyExecute(t1, t2, operation)
          case "*" => (h1 * h2) #:: lazyExecute(t1, t2, operation)
          case "/" => (h1.toDouble / h2.toDouble).asInstanceOf[A] #:: lazyExecute(t1, t2, operation)
          case _ => LazyList.empty
      case _ => LazyList.empty


  //TEST - ZADANIE 2
  println("\n\n***ZADANIE 1 - TEST***")
  println("Wyswietlam 5 elementow listy nr 1: ")
  lList1.take(5).foreach(println)
  println("Wyswietlam 5 elementow listy nr 2")
  lList2.take(5).foreach(println)

  println("Wyswietlam 10 elementow wykonania lazyExecute(lList, slList, '+')")
  lazy val resultList2 = lazyExecute(lList1, lList2, "+")
  resultList2.take(10).foreach(println)
  println("Wyswietlam 10 elementow wykonania lazyExecute(lList, slList, '*')")
  lazy val resultList3 = lazyExecute(lList1, lList2, "*")
  resultList3.take(10).foreach(println)

  //ZADANIE 3
  def duplicate[A](toDuplicate: LazyList[A], duplicateInts: LazyList[Int]): LazyList[A] =
    def duplicateIter(toDuplicate: LazyList[A], duplicateInts: LazyList[Int], counter: Int): LazyList[A] =
      if (counter == 0)
        (toDuplicate, duplicateInts) match
          case (h1 #:: t1, h2 #:: t2) => duplicateIter(t1, t2, h2)
          case (_) => LazyList.empty
      else
        toDuplicate.head #:: duplicateIter(toDuplicate, duplicateInts, counter - 1)

    duplicateIter(toDuplicate, duplicateInts.tail, duplicateInts.head)

  //ZADANIE 3 - TEST
  println("\n\n***ZADANIE 3 - TEST***")
  println("Wyswietlam 5 elementow listy lList: ")
  lList1.take(5).foreach(println)
  println("Wyswietlam wynik duplicate(lList, LazyList(0, 3, 1)).take(5).foreach(println)")
  duplicate(lList1, LazyList(0, 3, 1)).take(5).foreach(println)

  //ZADANIE 4 I 5
  trait Debug:
    def debugName(): String =
      getClass.getSimpleName

    def debugVars() =
      getClass.getDeclaredFields.toList.map(field =>
        field.setAccessible(true)
        (field.getName, field.getType, field.get(this))
      )

  case class Foo() extends Debug
  {
    var x: Int = 1
    var y: Int = 0
    var a: String = "test"
  }

  //TESTY - ZADANIE 4 I 5
  println("\n\n***ZADANIE 4,5 - TESTY")

  var f: Foo = new Foo()

  println("To jest klasa: " + f.debugName())
  println("Pola klasy: " + f.debugVars())
}
