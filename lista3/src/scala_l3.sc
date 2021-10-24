//1
def splitBySign(list: List[Int]): (List[Int], List[Int]) = {
  def negativeList(list: List[Int]): List[Int] = {
    if list == Nil then Nil
    else if list.head < 0 then list.head::negativeList(list.tail)
    else negativeList(list.tail)
  }
  def positiveOddList(list: List[Int]): List[Int] = {
    if list == Nil then Nil
    else if list.head > 0 && list.head % 2 == 1 then list.head::positiveOddList(list.tail)
    else positiveOddList(list.tail)
  }
  (negativeList(list), positiveOddList(list))
}

splitBySign(List(-3, -6, 7, -9, 13))
splitBySign(List(1, 2, -3, 0, 5))
splitBySign(Nil)


//2
def lengthOfList[A](list: List[A]): Int = {
  if list == Nil then 0
  else lengthOfList(list.tail) + 1
}

lengthOfList(List(5, 4, 3, 2))
lengthOfList(List("Ala", "ma", "kota"))
lengthOfList(Nil)


//3
def joinLists[A](list1: List[A], list2: List[A]): List[A] = {
  (list1, list2) match
    case (Nil, Nil) => Nil
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (h1::t1, h2::t2) => h1::h2::joinLists(t1, t2)
}

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))
joinLists(List('a', 'c', 'd'), List('b'))
joinLists(Nil, List(1, 2, 3))
