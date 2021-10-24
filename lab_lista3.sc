//lista 3 (Scala)

//zadanie 1
def splitBySign (l: List[Int]): (List[Int], List[Int]) =
{
  l match {
    case h::t => {
      val (l1, l2) = splitBySign(t)

      if h<0 then (h::l1, l2)
      else if h>0 && h%2!=0 then (l1, h::l2)
      else (l1, l2)
    }
    case Nil => (Nil, Nil)
  }
}

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(4, -5, 6, -7, -8, 9)) == (List(-5, -7, -8), List(9))
splitBySign(List(0)) == (List(), List())
splitBySign(List(0, 2, 4, 6, 8)) == (List(), List())
splitBySign(List(1, 3, 5, 7, 9)) == (List(), List(1, 3, 5, 7, 9))
splitBySign(List(-5, -4, -3, -2, -1, 0)) == (List(-5, -4, -3, -2, -1), List())
splitBySign(List(-7, 14, 13, -4, 2, 4)) == (List(-7, -4), List(13))
splitBySign(List()) == (List(), List())

//zadanie 2
def lengthOfList[A] (l: List[A]): Int =
{
  if l == Nil then 0
  else 1+lengthOfList(l.tail)
}

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List('a', 'a', 'b', 'a')) == 4
lengthOfList(List(1.02)) == 1
lengthOfList(List(List(true, false, false), List(false, true))) == 2
lengthOfList(List()) == 0

//zadanie 3
def joinLists[A] (l1: List[A], l2: List[A]): List[A] =
{
  (l1, l2) match {
    case (Nil, l2) => l2
    case (l1, Nil) => l1
    case (h1::t1, h2::t2) => h1::h2::joinLists(t1, t2)
  }
}

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(-5, -4, -3, -2, -1), List(5, 4, 3, 2, 1, 0)) == List(-5, 5, -4, 4, -3, 3, -2, 2, -1, 1, 0)
joinLists(List('c', 'b', 'a'), List('b', 'c')) == List('c', 'b', 'b', 'c', 'a')
joinLists(List(), List(true)) == List(true)
joinLists(List(), List()) == List()

