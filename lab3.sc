def splitBySign (xs: List[Int]): (List[Int], List[Int]) =
  def splitBySignIter(list: List[Int], negative: List[Int], oddPostive: List[Int]): (List[Int], List[Int]) =
    list match {
      case Nil => (negative, oddPostive)
      case h :: t if h < 0 => splitBySignIter(t, negative ++ List(h), oddPostive)
      case h :: t if h % 2 != 0 => splitBySignIter(t, negative, oddPostive ++ List(h))
      case _ => splitBySignIter(list.tail, negative, oddPostive)
  }
  splitBySignIter(xs, Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 12))
splitBySign(List(-3, -6, 7, -9, 13))
splitBySign(List(-3, -6, 7, -9, 0, 0, 0))
splitBySign(Nil)


def lengthOfList (list: List[Int]): Int =
  list match {
    case Nil => 0
    case h :: t => 1 + lengthOfList(t)
  }

lengthOfList(List(-3, -6, 7, -9, 12))
lengthOfList(List(-3, -6, 7, -9, 13))
lengthOfList(List(-3, -6, 7, -9, 0, 0, 0))
lengthOfList(Nil)


def joinLists(list1: List[Int], list2: List[Int]): List[Int] =
  (list1, list2) match {
    case (Nil, Nil) => Nil
    case (_, Nil) => list1
    case (Nil, _) => list2
    case (h1 :: t1, h2 :: t2) => h1 :: h2 :: joinLists(t1, t2)
  }

joinLists(List(5,4,3,2), List(1,2,3,4,5,6))
joinLists(List(-3, -6, -7, -9), List(11, 22, 33, 44))
joinLists(Nil, List(-3, -6, 7, -9, 12))
joinLists(List(-3, -6, 7, -9, 12), Nil)
joinLists(List(-3, -6, 7, -9, 12), List(1,2))
joinLists(List(-3, -6, 7, -9, 12), List(-1))
