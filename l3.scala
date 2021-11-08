object l3 {
  //1
  //zl. obl. = l
  //zl. pam. = l
  def reverse(list: List[Int], result: List[Int]): (List[Int]) =
    list match
      case Nil => result
      case (hd :: tl) => reverse(tl, hd :: result)

  def splitBySign(list: List[Int]): (List[Int], List[Int]) =
    def split(list: List[Int], neg: List[Int], pos: List[Int]): (List[Int], List[Int]) =
      list match
        case Nil => (reverse(neg, Nil), reverse(pos, Nil))
        case (hd :: tl) if hd < 0 => split(tl, hd :: neg, pos)
        case (hd :: tl) if hd % 2 != 0 => split(tl, neg, hd :: pos)
        case (hd :: tl) => split(tl, neg, pos)
    split(list, Nil, Nil)

  //2
  //zl. obl. = l
  //zl. pamieciowa = l, gdyby bylo ogonowo to 1
  def lengthOfList[A](list: List[A]): Int =
    if list == Nil then 0
    else 1 + lengthOfList(list.tail)

  //3
  //zl. obl. = dl. krotszej listy + 1
  //zl pamieciowa = 2 + 4 + ... + 2n(dl. krotszej listy) + 2n + (dl. dluzszej listy-dl. krotszej listy)
  def joinLists[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match
      case (h1::t1, h2::t2) => h1 :: h2 :: joinLists(t1, t2)
      case (Nil, _) => list2
      case ( _, Nil) => list1


  def main(args: Array[String]): Unit = {
    println(splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13)))
    println(splitBySign(List()) == (List(), List()))
    println(splitBySign(List(0, 2, 4, 6, 8)) == (List(), List()))

    println(lengthOfList(List(1, 2, 3, 4)) == 4)
    println(lengthOfList(Nil) == 0)
    println(lengthOfList(List(List(List()), List(List()), List(List()))) == 3)

    println(joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2 ,4, 5, 6))
    println(joinLists(List(), List()) == List())
    println(joinLists(List(), List('a', 'b', 'c')) == List('a', 'b', 'c'))
    println(joinLists(List(1, 1, 1, 1, 1, 1, 1), List(2, 2, 2, 2, 2, 2)) == List(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1))
  }
}
