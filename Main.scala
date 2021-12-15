object Main {

  /*
    W zadaniach 1-3 uzylem LazyList, poniewaz przechowywane w niej wartosci sa ewaluowane leniwie(sa obliczane tylko wtedy, kiedy sa potrzebne)
    Dlaczego nie Stream, ktory dalby bardzo podobny efekt?
    Glownie dlatego, ze Stream jest @deprecated. To oznacza, ze w przyszlosci moze zostac usuniety.
    Pozatym Lazylist jest bardziej leniwa i w przeciwienstwie do Stream'a nie ma obliczonej wartosci glowy, co moze zmniejszyc ilosc bledow/pomylek podczas
    operowania na tej strukturze.

    Spojrzmy teraz na konkretne zadania i zalety plynace z uzycia w nich LazyList:
    -Zadanie 1:
    Jesli otrzymana lista jest bardzo duza, a element konczacy maly, w stosunku do tej list, to nie potrzebujemy znac calej list. Przeprowadzimy
    operacje tylko na poczatkowych elementach oszczedzajac czas i pamiec.
    -Zadanie 2:
    Poniewaz w tym zadaniu dochodza pewne obliczenie(co prawda proste, ale jednak), to zleniwienie oznacza, ze nie musimy zuzywac zasobow na liczenie
    nieuzywanych elementow.
    -Zadanie 3:
    Podobnie jak w zadaniu 1, lista moze byc bardzo dluga, a lista z iloscia powtorzen krotka. Dzieki liscie leniwej nie operujemy na zbednych wartosciach.
  */

  def main(args: Array[String]): Unit =

    //task 1 - test
    //eachNElement(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), -3, 8).take(10) //throws exception
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3, 8).take(10) == List(1, 4, 7))
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1, 5).take(10) == List(1, 2, 3, 4, 5))
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2, 10).take(3) == List(1, 3, 5))
    println()

    //task 2 - test
    println(lazyExecute(LazyList(1, 2, 3, 4), LazyList(2, 5), '+').take(6) == List(3, 7, 3, 4))
    println(lazyExecute(LazyList(1, 2, 3, 40), LazyList(2, 5, 8, 8), '-').take(6) == List(-1, -3, -5,32))
    println(lazyExecute(LazyList(1, 2, 3, 4), LazyList(2, 5, 3), '*').take(6) == List(2, 10, 9, 4))
    println(lazyExecute(LazyList(1, 2, 30, 45), LazyList(2, -5, 4, 5), '/').take(6) == List(0, 0, 7, 9))
    println()

    //task 3 - test
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).take(10) == List(2, 2, 2, 3))
    println(duplicate(LazyList(1, 2, 3, 4, 6, 5, 10), LazyList(0, -3, 2, 4, -1, 3, 10)).take(10) == List(3, 3, 4, 4, 4, 4, 5, 5, 5, 10))
    println(duplicate(LazyList('a', 'n', 'b'), LazyList(0, 3, -1, 4)).take(10) == List('n', 'n', 'n'))
    println()

    //task 4 & 5 - test
    val point: Point = new Point(2, 5)
    println(point.debugName())
    println(point.debugVars())
    println()


  //task 1
  def eachNElement[T](inputList: LazyList[T], gap: Int, end: Int): LazyList[T] =

    def helper[T](restList: LazyList[T], gapCounter: Int, elemCounter: Int): LazyList[T] =
      if elemCounter == end then LazyList.empty
      else
        restList match
          case LazyList() => LazyList.empty
          case head #:: tail if gapCounter == gap => head #:: helper(tail, 1, elemCounter + 1)
          case head #:: tail => helper(tail, gapCounter + 1, elemCounter + 1)
    if gap <= 0 || end <= 0 then throw new Exception("Invalid value")
    else helper(inputList, gap, 0)

  //task 2
  def lazyExecute(firstList: LazyList[Int], secondList: LazyList[Int], operator: Char): LazyList[Int] =

    (firstList, secondList) match
      case (LazyList(), LazyList()) => LazyList.empty
      case (LazyList(), _) => secondList
      case (_, LazyList()) => firstList
      case (h1 #:: t1, h2 #::t2) => doOperation(h1, h2, operator) #:: lazyExecute(t1, t2, operator)

  def doOperation(a: Int, b: Int, operator: Char): Int =

    operator match
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case '/' =>
        if b == 0 then throw new Exception("Division by zero")
        else a / b
      case _ => throw new Exception("Unknown operator")

  //task 3
  def duplicate[T](list: LazyList[T], duplicatesNr: LazyList[Int]): LazyList[T] =

    def duplicateHelper[T](list: LazyList[T], duplicatesNr: LazyList[Int], counter: Int): LazyList[T] =
      (list, duplicatesNr) match
        case (h1 #:: _, h2 #:: _) if counter < h2 => h1 #:: duplicateHelper(list, duplicatesNr, counter + 1)
        case (_ #:: t1, _ #:: t2) => duplicateHelper(t1, t2, 0)
        case _ => LazyList.empty
    duplicateHelper(list, duplicatesNr, 0)

  //task 4 & 5
  trait Debug:
    def debugName(): String =
      getClass.getSimpleName

    def debugVars(): List[Any] =
      getClass.getDeclaredFields.toList.map(field =>
        field.setAccessible(true)
        List(field.getName, field.getType, field.get(this)))

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

}
