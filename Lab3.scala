object Lab3 {
  //zadanie1
  def splitBySign(xs:List[Int]):(List[Int],List[Int])= {
    def splitBySignHelper(xs: List[Int], negative: List[Int], positive: List[Int]): (List[Int], List[Int]) = {
      xs match {
        case Nil => (negative, positive)
        case h::t => if(h < 0) then splitBySignHelper (t, negative:::List(h), positive  )
      else if (h > 0 && h % 2 != 0) then splitBySignHelper (t, negative, positive:::List(h))
      else splitBySignHelper (t, negative, positive)
      }
    }

    splitBySignHelper(xs, List(), List())
  }

  //złożoność obliczeniowa O(n^2)
  //złożoność pamięciowa O(n)

  //zadanie2
  def lengthOfList[A] (xs:List[A]):Int = {
    if (xs == Nil) then 0 else
      1 + lengthOfList(xs.tail)
  }

  //złożoność obliczeniowa O(n)
  //złożoność pamięciowa O(n)

  //zadanie3
    def joinLists[A] (xs1: List[A], xs2: List[A]) : List[A]= {
      (xs1, xs2) match{
        case (Nil,Nil) => Nil
        case (h1::t1, h2::t2) => h1::h2::joinLists(t1, t2)
        case (_, Nil) => xs1
        case (Nil, _) => xs2
      }
    }

  //złożoność obliczeniowa O(n)
  //złożoność pamięciowa O(n)

  def main(args: Array[String]): Unit = {

    println(splitBySign(List(-3, -1, 2, 0, 3, 5)))
    println(splitBySign(List(1, 2, 3, 4, 5)))
    println(splitBySign(List(-5, -4, -4, 2)))
    println(splitBySign(List()))

    println(lengthOfList(List("a", "l", "a")))
    println(lengthOfList(List(2, 3, 2, 12, 9, 2)))
    println(lengthOfList(List("a", 0, 2, "ab", "a")))
    println(lengthOfList(List()))

    println(joinLists(List(1, 2, 3, 4), List(5, 6, 7, 8)))
    println(joinLists(List(1), List(5, 6, 7, 8, 9)))
    println(joinLists(List(1, 2, 3, 4, 5), List(6)))
    println(joinLists(List(), List(5, 6, 7)))
    println(joinLists(List(1, 2, 3, 4), List()))
    println(joinLists(List(), List()))
  }
}
