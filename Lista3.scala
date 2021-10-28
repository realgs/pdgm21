object Lista3 {

  //pomocnicze funkcje
    def addToList[A] (xs: List[A], x: A): List[A] =
      if xs != Nil
      then xs.head :: addToList(xs.tail, x)
      else List(x)

    def addToList2[A] (xs: List[A], x: A, y: A): List[A] =
      if xs != Nil
      then xs.head :: addToList2(xs.tail, x, y)
      else List(x, y)


    //zadanie 1

    def splitBySign (xs: List[Int]): (List[Int], List[Int]) =
      def splitter(xs: List[Int], xsNeg: List[Int], xsPosOdd: List[Int]): (List[Int], List[Int]) =
        if xs == Nil
          then (xsNeg, xsPosOdd)
        else if xs.head < 0
          then splitter(xs.tail, addToList(xsNeg, xs.head), xsPosOdd)
        else if (xs.head > 0 && xs.head % 2 != 0)
          then splitter(xs.tail, xsNeg, addToList(xsPosOdd, xs.head))
        else splitter(xs.tail, xsNeg, xsPosOdd)
      splitter(xs, List(), List())



    //Zadanie 2

    def joinLists(xs1: List[Int], xs2: List[Int]): List[Int] =
      def joinHelper(xs1: List[Int], xs2: List[Int], xsResult: List[Int]): List[Int] =
        (xs1, xs2) match {
          case (Nil, Nil) => xsResult
          case (Nil, _) => joinHelper(xs1, xs2.tail, addToList(xsResult, xs2.head))
          case (_, Nil) => joinHelper(xs1.tail, xs2, addToList(xsResult, xs1.head))
          case (_, _) => joinHelper(xs1.tail, xs2.tail, addToList2(xsResult, xs1.head, xs2.head))
        }
      joinHelper(xs1, xs2, List())


    //Zadanie 3

    def lengthOfList[A](xs: List[A]): Int =
      if xs != Nil
      then 1 + lengthOfList(xs.tail)
      else 0



    def main(args: Array[String]) : Unit = {
      println("Zadanie 1")
      println(splitBySign(List(-3, -6, 7, -9, 13)));
      println(splitBySign(List(-2, 4, 5, 0, 5, 13)));

      println("Zadanie 2")
      println(lengthOfList(List(5, 4, 3, 2)))
      println(lengthOfList(List()))
      println(lengthOfList(List(1, 2, 3, 4, 5, 6)))
      println(lengthOfList(List("k", "o", "t")))

      println("Zadanie 3")
      println(joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6 )))
      println(joinLists(List(1, 1, 1, 1, 1, 1, 1, 1), List(2, 2, 2, 2 )))
  }
}
