import lista6.Debug

import math.Numeric.Implicits.infixNumericOps

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object lista6 {

  //Zadanie 1
  def eachNElement[A](gapElem: Int, endElem: Int, list: LazyList[A]): LazyList[A] =
    def helper[A](list: LazyList[A], passedElemCounter: Int, gapCounter: Int): LazyList[A] =
      if passedElemCounter == endElem then LazyList[A]()
      else
        (list, gapCounter) match
          case (h #:: t, counter) =>
            if counter == 0 then list.head #:: helper(t, passedElemCounter + 1, gapElem - 1)
            else helper(t, passedElemCounter + 1, gapCounter - 1)
          case (Nil #:: _, _) => LazyList[A]()

    if list == Nil then LazyList[A]()
    else if gapElem > 0 && endElem > 0 then list.head #:: helper(list.tail, 1, gapElem - 1)
    else LazyList[A]()

  //Zadanie 2
  def lazyExecute[A: Numeric](list1: LazyList[A], list2: LazyList[A], sign: Char): LazyList[A] =
    (list1, list2) match
      case (h1 #:: t1, h2 #:: t2) =>
        sign match
          case '+' => (h1 + h2) #:: lazyExecute(t1, t2, sign)
          case '-' => (h1 - h2) #:: lazyExecute(t1, t2, sign)
          case '*' => (h1 * h2) #:: lazyExecute(t1, t2, sign)
          case '/' => (h1.toDouble / h2.toDouble).asInstanceOf[A] #:: lazyExecute(t1, t2, sign)
          case _ => LazyList[A]()
      case (h1 #:: t1, _) => h1 #:: lazyExecute(t1, LazyList(), sign)
      case (_, h2 #:: t2) => h2 #:: lazyExecute(LazyList(), t2, sign)
      case _ => LazyList[A]()

  //Zadanie 3
  def duplicate[A](elemToDuplicate: LazyList[A], duplicators: LazyList[Int]): LazyList[A] =
    def helper[A](restOfElem: LazyList[A], reps: Int, restOfDUplicators: LazyList[Int]): LazyList[A] =
      (restOfElem, restOfDUplicators) match
        case (h1 #:: t1, h2 #:: t2) =>
          if h2 < 0 then
            println("Liczba powtórzeń mniejsza od zera dla elementu: " + h1)
            helper(t1, 0, t2)
          else if reps < h2 then h1 #:: helper(restOfElem, reps + 1, restOfDUplicators)
          else helper(t1, 0, t2)
        case _ => LazyList[A]()

    if elemToDuplicate == LazyList[A]() || duplicators == LazyList[A]() then LazyList[A]()
    else helper(elemToDuplicate, 0, duplicators)

  //Zadanie 4 i 5

  trait Debug {
    def debugName(): String =
      getClass.getSimpleName

    def debugVars(): List[List[Object]] =
      getClass.getDeclaredFields.toList.map { f =>
        f.setAccessible(true)
        val result = List(f.getName, f.getType, f.get(this))
        f.setAccessible(false)
        result
      }
  }

  def main(args: Array[String]): Unit = {
    //Zadanie 1
    println("Zadanie 1")
    println(eachNElement(2, 3, LazyList(5, 6, 3, 2, 1)).take(10).toList)
    println(eachNElement(2, 4, LazyList(5, 6, 3, 2, 1)).take(10).toList)
    println(eachNElement(1, 6, LazyList(5, 6, 3, 2, 1, 10, 9, 14)).take(10).toList)
    println(eachNElement(0, 6, LazyList(5, 6, 3, 2, 1, 10, 9, 14)).take(10).toList)
    println(eachNElement(-1, 6, LazyList(5, 6, 3, 2, 1, 10, 9, 14)).take(10).toList)
    println(eachNElement(3, 6, LazyList(5, 6, 3, 2, 1, 10, 9, 14)).take(10).toList)
    println(eachNElement(3, 0, LazyList(5, 6, 3, 2, 1, 10, 9, 14)).take(10).toList)
    println(eachNElement(3, -1, LazyList(5, 6, 3, 2, 1, 10, 9, 14)).take(10).toList)
    //Zadanie 2
    println("\nZadanie 2")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '/').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), ';').take(10).toList)
    //Zadanie 3
    println("\nZadanie 3")
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).take(10).toList)
    println(duplicate(LazyList('a', 'c', 'd', 'b'), LazyList(1, 4, 2, 1)).take(10).toList)
    println(duplicate(LazyList('a', 'c', 'd', 'b'), LazyList()).take(10).toList)
    println(duplicate(LazyList(), LazyList(1, 4, 2, 1)).take(10).toList)
    println(duplicate(LazyList('a', 'c', 'd', 'b'), LazyList(-1, 4, -2, 1)).take(10).toList)

    var p: Point = new Point(3, 4)
    //Zadanie 4
    println("\nZadanie 4")
    println(p.debugName())
    //Zadanie 5
    println("\nZadanie 5")
    println(p.debugVars())
  }
}
