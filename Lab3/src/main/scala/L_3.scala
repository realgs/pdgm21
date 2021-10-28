object L_3 {

  //zadanie 1
  def splitBySign(xs: List[Int]): (List[Int], List[Int]) =
    xs match {
      case (Nil) => (Nil, Nil)
      case (h::t) =>
        if h < 0 then (h::splitBySign(t)._1, splitBySign(t)._2)
        else if h > 0 && h % 2 == 1 then (splitBySign(t)._1, h::splitBySign(t)._2)
        else (splitBySign(t)._1, splitBySign(t)._2)
    }

  //zadanie 2
  def lengthOfList[A](xs: List[A]): Int =
    xs match {
      case (Nil) => 0
      case (h::t) => 1 + lengthOfList(t)
    }

  //zadanie 3
  def joinLists[A](xs: List[A], ys: List[A]): List[A] =
    (xs, ys) match {
      case (Nil, Nil) => Nil
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (h1::t1, h2::t2) => h1::joinLists(h2::t2, t1)
    }

  def main(args: Array[String]): Unit = {
    println(splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13)))
    println(splitBySign(List()) == (List(),List()))
    println(splitBySign(List(1, 2, 3, -4, -5, 6, 7)) == (List(-4, -5), List(1, 3, 7)))

    println(lengthOfList(List(5, 4, 3, 2)) == 4)
    println(lengthOfList(List()) == 0)
    println(lengthOfList(List(List("a", "b", "c"), List("q", "w", "e", "r", "t", "y"))) == 2)

    println(joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3 ,2, 4, 5, 6))
    println(joinLists(List(), List()) == List())
    println(joinLists(List(1, 2, 3), List()) == List(1, 2, 3))
    println(joinLists(List(), List(1, 2, 3)) == List(1, 2, 3))
    println(joinLists(List(1, 3, 5, 7), List(2, 4, 6, 8)) == List(1, 2, 3, 4, 5, 6, 7, 8))
  }
}
