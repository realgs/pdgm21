object l6 {

//  def reverse[A](lazyList: LazyList[A]): LazyList[A] = {
//    def reverseIter(toReverse: LazyList[A], result: LazyList[A]): LazyList[A] =
//      toReverse match
//        case LazyList() => result
//        case h #:: t => reverseIter(t, h #:: result)
//    reverseIter(lazyList, LazyList())
//  }

//task1
  def eachNElement[A](list: LazyList[A], element: Int, endIndex: Int): LazyList[A] =
    def eachNElementIter[A](list: LazyList[A], currentIndex: Int, result: LazyList[A]): LazyList[A] =
      (currentIndex, list) match
        case (i, h #:: t)     => if i == endIndex then result
                                else if i % element == 0 then eachNElementIter(t, i+1, h #:: result)
                                else eachNElementIter(t, i+1, result)
        case (_, LazyList())  => result
    if element <= 0 then throw new Exception("Invalid index value")
    if endIndex < 0 then throw new Exception("Invalid end index value")
//    reverse(eachNElementIter(list, 0, LazyList()))
    eachNElementIter(list, 0, LazyList()).reverse

  //task2
  def operation(elem1: Int, elem2: Int, operator: Char): Int =
    operator match
      case '+' => elem1 + elem2
      case '-' => elem1 - elem2
      case '*' => elem1 * elem2
      case '/' => if elem2 == 0 then throw new Exception("Division by 0") else elem1 / elem2
      case _   => throw new Exception("Operation unsupported")

  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operator: Char): LazyList[Int] =
    def lazyExecuteIter(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match
        case (LazyList(), _) => list2
        case (_, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => operation(h1, h2, operator) #:: lazyExecuteIter(t1, t2)
    lazyExecuteIter(list1, list2)

    //task3
  def duplicate[A](listToDuplicate: LazyList[A], instructionList: LazyList[Int]): LazyList[A] =
    def duplicateIter[A](toDuplicate: LazyList[A], listOfRepeats: LazyList[Int]): LazyList[A] =
      (toDuplicate, listOfRepeats) match
        case (_ #:: t1, 0 #:: t2) => duplicateIter(t1, t2)
        case (h1 #:: _, h2 #:: t2) => h1 #:: duplicateIter(toDuplicate, (h2-1) #:: t2)
        case _  => LazyList()
    duplicateIter(listToDuplicate, instructionList)

  /*
  W zadanich 1.-3. uzylam list leniwych - LazyList. Nie uzylam strumieni, poniewaz sa przestarzale i nie są w pełni leniwe - ich glowa jest wartosciowana gorliwie, a ogon leniwie.
  LazyList gwarantuje wartosciowanie w pelni leniwe, tzn. wartosc kolejnego elementu jest wyznaczana, jezeli jest to konieczne.
  W zadaniu 1. jezeli podany do funkcji indeks konczacy jest maly, a podana lista leniwa zawiera bardzo duzo elementow,
  to wartosci powyzej indeksu konczacego, ktore nie beda nam potrzebne, nie zostana zewaluowane i bedzie to bardziej optymalne rozwiazanie. 
  Podobnie w 2. zadaniu, uzycie list leniwych m.in. oszczedza uzywana pamiec. Jezeli jedna lista sie konczy, dalsza czesc drugiej nie jest ewaluowana.
  W zadaniu 3. jesli znalabym ilosc danych wejsciowych zastanowilabym sie nad uzyciem innej struktury.
  Np. jezeli lista instrucionList zawierajaca ilosc powtorzen danych elementow jest krotka, to zyskujemy uzywajac listy leniwej, poniewaz nie ewaluujemy niepotrzebnych wartosci.
  */
  
  //task4;5
  trait Debug {
    def debugName(): String =
      getClass().getSimpleName()

//    def debugVars(): List[Any] =
//      getClass.getDeclaredFields.toList.map(f =>
//        f.setAccessible(true)
//        List(f.getName(), f.getType(), f.get(this)))

    def debugVars() =
      val list = this.getClass.getDeclaredFields.toList
        def debugVarsIter(list: List[java.lang.reflect.Field]): List[List[Any]] =
          list match
            case Nil => Nil
            case h :: t =>
              h.setAccessible(true)
              List(h.getName(), h.getType(), h.get(this)) :: debugVarsIter(t)
        debugVarsIter(list)
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Card(no: BigInt, name: String) extends Debug {
    var number: BigInt = 1234
    var owner: String = "default"
  }


  def main(args: Array[String]): Unit = {
    println("eachNElement")
    println(eachNElement(LazyList(5,6,3,2,1),2,3).toList) //5,3
    println(eachNElement(LazyList(5),1,3).toList) //5
    println(eachNElement(LazyList(5,6,3,2,1),2,4).toList) //5,3
    println(eachNElement(LazyList(5,6,3,2,1),1,0).toList) //5,6,3,2
    println(eachNElement(LazyList(5,6,3,2,1),1,4).toList) //5,6,3,2
    println(eachNElement(LazyList('a','b','c','d'),3,4).toList) //a,d
//    println(eachNElement(LazyList(5,6,3,2,1),2,-2).toList)
//    println(eachNElement(LazyList(5,6,3,2,1),-3,0).toList)
    println("lazyExecute")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList) //[3,5,7,5]
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList) //[2,6,12,5]
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList) //[-1,-1,-1,5]
    println(lazyExecute(LazyList.from(10), LazyList.from(1), '+').take(3).toList) //[11,13,15]
    println(lazyExecute(LazyList.from(1), LazyList(2,3,4,5), '+').take(50).toList)
//    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 0, 4, 5), '/').toList)
    println("duplicate")
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList) //[2,2,2,3]
    println(duplicate(LazyList(1, -2, 0), LazyList(3, 3)).toList)
    println(duplicate(LazyList.from(1), LazyList(3, 3)).take(5).toList)

    println("debugName & debugVars")
    var p : Point = new Point(3, 4)
    println(p.a)
    println(p.debugName())
    println(p.debugVars())

    var c : Card = new Card(1234123439, "ownerName")
    println(c.debugVars())
  }
}
